{
    grupowanie [] = [];
    grupowanie(l:ls) = grp [l] ls where
        grp acc [] = [acc];
        grp acc@(a:as) (l:ls)
            |a==l =grp (l:acc) ls
            |otherwise = [acc]++ grp [l] ls
        ;
}
