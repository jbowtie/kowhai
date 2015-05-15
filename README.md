# Kowhai

Kowhai is an Earley-style parser loosely based on the published MARPA algorithm. Currently it is in an experimental state but should handle all LR grammars.

A `kowhai.Grammar` is used to build up a ruleset using a API that maps well to typical BNF constructs. Once a top-level start rule is identified, it can build
a split-epsilon finite state machine.

A `kowhai.Parser`, given a stream of tokens (perhaps from a lexer) and a state machine, will produce a parse tree that can then be used to build an astract syntax tree.

