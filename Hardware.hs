module Hardware where

import Control.Concurrent.STM

type Lab = String

data Port = Up | Down | Left | Right | Any
          deriving Show

type Loc = Maybe Port -- Nothing means Acc

acc :: Loc
acc = Nothing

data Instruction = JMP Lab | AddLit Int | SubLit Int | AddPort Port | SubPort Port
                 | JEZ Lab | JGZ Lab | JLZ Lab | JNZ Lab | JRO Int | NOP | MOV Loc Loc | MovInt Int Loc
                 | SAV | SWP
                 deriving Show
-- I'm not completely confident yet that this covers all of the MOV cases properly, but I think it does

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

toLab :: Lab -> Node -> Node
toLab l n = undefined

fromAny :: Node -> STM Int
fromAny n = undefined

toAny :: Node -> Int -> STM Int
toAny n i = undefined

readPort :: Port -> Node -> STM Int
readPort p n = undefined

interpStep :: Instruction -> Node -> STM Node
interpStep (AddLit i) n = let oldAcc = acc n 
                      in return $ n{acc = oldAcc + i}
interpStep (SubLit i) n = let oldAcc = acc n
                      in return $ n{acc = oldAcc -1}
interpStep (AddPort d) n = do
  i <- readPort d n
  return $ n{ acc = (acc n) + i}
interpStep (SubPort d) n = interpStep (AddPort (-d)) -- I'm lazy
