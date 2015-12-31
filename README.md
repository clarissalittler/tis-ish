# tis-ish
TIS-100 inspired shenanigans
# high-level language
One thing to try is a high-level message passing language that will compile down to our TIS-ish machine eventually
# low-level language
The same way we can have an abstracted assembly language that then gets compiled down to real assembly, we could have a slightly higher level language that compiles down to "real" assembly and nodes. 

For example, in our abstract asm we can lift the 15-instruction restriction and, perhaps, have arbitrary message
passing between nodes. Then it would compile down to a hardware configuration that appropriately connects all of the nodes in the proper geometry, possibly being non-planar or just throwing an error if it turns out that a non-planar graph would be needed.
# hardware simulation and assembly
The base TIS instructions are pretty simple, as are the node representations.

I think if I want to do something cutesy I can just use STM and concurrent execution to simulate the "hardware" itself.
