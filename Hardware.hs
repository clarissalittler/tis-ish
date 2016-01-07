module Hardware where

import Prelude hiding (Either, Left, Right)
import Control.Concurrent.STM
import Data.List

type Lab = String

data Port = Up | Down | Left | Right | Any
          deriving Show

type Loc = Maybe Port -- Nothing means Acc

-- one thing I want to add into this, and am not quite sure how, is the ability for these nodes to be programmable
-- like what I want is for us to have a way to take a network of nodes and feed instructions into them.
-- maybe this will all make more sense to me after I spend more time studying instruction sets
data Instruction = JMP Lab | AddLit Int | SubLit Int | AddPort Port | SubPort Port
                 | JEZ Lab | JGZ Lab | JLZ Lab | JNZ Lab | JRO Int | NOP | MOV Loc Loc | MovInt Int Loc
                 | SAV | SWP | LAB Lab
                 deriving Show
-- I'm not completely confident yet that this covers all of the MOV cases properly, but I think it does

-- one thing I haven't figured out is how I want to synchronize stacks together
-- what we need is for the stack to be modified any time the TMVar is filled.
-- I supposed the easiest way is for the stack to have its own simulation step in which it empties the in
-- TMVar, pushing that into the stack and then pop from the stack into the out channel. Of course that doesn't
-- quite work does it? Hrmmm 
-- I feel like we perhaps need a separate instruction for popping and pushing to a stack
-- I'm not sure if that's cheap, though. 
data StackNode = Stack { stack :: [Int]}

data Node = Node {leftIn :: TMVar Int,
                  rightIn :: TMVar Int,
                  downIn :: TMVar Int,
                  upIn :: TMVar Int,
                  leftOut :: TMVar Int,
                  rightOut :: TMVar Int,
                  upOut :: TMVar Int,
                  downOut :: TMVar Int,
                  acc :: Int,
                  bak :: Int,
                  programCounter :: Int,
                  insts :: [Instruction]
                  }

findLab :: Lab -> [Instruction] -> Maybe Int
findLab l is = findIndex aux is
    where aux i = case i of
                     LAB l' -> l == l'
                     _ -> False

toLab :: Lab -> Node -> Maybe Node
toLab l n = case findLab l (insts n) of
              Nothing -> Nothing
              Just i -> Just n{programCounter = i}

fromAny :: Node -> STM Int
fromAny n = takeTMVar (leftIn n) `orElse` 
            takeTMVar (rightIn n) `orElse`
            takeTMVar (upIn n) `orElse`
            takeTMVar (downIn n)

toAny :: Node -> Int -> STM ()
toAny n i = putTMVar (leftOut n) i `orElse` 
            putTMVar (rightOut n) i `orElse`
            putTMVar (upOut n) i `orElse`
            putTMVar (downOut n) i

readPort :: Port -> Node -> STM Int
readPort Up n = takeTMVar (upIn n)
readPort Down n = takeTMVar (downIn n)
readPort Left n = takeTMVar (leftIn n)
readPort Right n = takeTMVar (rightIn n)
readPort Any n = fromAny n

writePort :: Port -> Int -> Node -> STM ()
writePort Up i n = putTMVar (upOut n) i
writePort Down i n = putTMVar (downOut n) i
writePort Left i n = putTMVar (leftOut n) i
writePort Right i n = putTMVar (rightOut n) i
writePort Any i n = toAny n i -- why do the args flip? why do I have this?

nextInst :: Node -> Node
nextInst n = if (programCounter n + 1 == (length $ insts n)) || (length $ insts n) == 0
             then n{programCounter = 0} else n{programCounter = (programCounter n)+1}

jmphelper' :: (Int -> Bool) -> Node -> Lab -> Maybe Node
jmphelper' c n l = let accVal = acc n 
                  in if (c accVal) then toLab l n else Just (nextInst n)

jmphelper c n l = case jmphelper' c n l of
                    Nothing -> error $ "no label: " ++ l
                    Just n' -> return n'

interpStep :: Instruction -> Node -> STM Node
interpStep (LAB l) n = return $ nextInst n
interpStep NOP n = return $ nextInst n
interpStep SWP n = let accVal = acc n
                       bakVal = bak n
                   in return $ nextInst $ n{acc = bakVal, bak = accVal}
interpStep SAV n = let accVal = acc n
                   in return $ nextInst $ n{bak = accVal}
interpStep (AddLit i) n = let oldAcc = acc n 
                      in return $ nextInst $ n{acc = oldAcc + i}
interpStep (SubLit i) n = let oldAcc = acc n
                      in return $ nextInst $ n{acc = oldAcc -1}
interpStep (AddPort d) n = do
  i <- readPort d n
  return $ nextInst $ n{acc = (acc n) + i}
interpStep (SubPort d) n = do
  i <- readPort d n
  return $ nextInst $ n{acc = (acc n) - i}
interpStep (MovInt i Nothing) n = return $ nextInst $ n{ acc = i}
interpStep (MovInt i (Just d)) n = (writePort d i n) >> (return $ nextInst n)
interpStep (MOV Nothing Nothing) n = error "I'm pretty sure this isn't allowed"
interpStep (MOV Nothing (Just d)) n = (writePort d (acc n) n) >> (return $ nextInst n)
interpStep (MOV (Just d) Nothing) n = do
  i <- readPort d n
  return $ nextInst $ n{acc = i}
interpStep (MOV (Just d) (Just d')) n = do
  i <- readPort d n
  writePort d' i n
  return $ nextInst n
interpStep (JMP l) n = case toLab l n of
                         Nothing -> error $ "could not find label: " ++ l
                         Just n -> return n
interpStep (JEZ l) n = jmphelper (==0) n l
interpStep (JGZ l) n = jmphelper (>0) n l
interpStep (JLZ l) n = jmphelper (<0) n l
interpStep (JNZ l) n = jmphelper (/=0) n l
interpStep (JRO i) n = let pCount = programCounter n
                           numInsts = length $ insts n
                           fix x | x < 0 = 0
                                 | x >= numInsts = numInsts -1
                       in return $ n{programCounter = (fix $ pCount + i)}


{- 
   So now we need code to actually create a map of nodes
-}
