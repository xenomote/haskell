module Expression where

    import Automata.Nondeterministic

    data Expression a = Empty 
        | Letter a 
        | Concat (Expression a) (Expression a)
        | Either (Expression a) (Expression a)
        | Repeat (Expression a)

    toNFA :: Expression a -> Automata s a
    toNFA = undefined