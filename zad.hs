{
    gcd1 n 0 = n;
    gcd1 n m = gcd m (n `mod` m);

    lcm1 n m = div (m * n ) (gcd n m);

    fib n = fib_acc 1 0 n 2 where
        fib_acc p1 p2 n x 
            | n<2 = n
            | x == n = p1 + p2
            | otherwise = fib_acc (p1 + p2) p1 n (x+1)
    ;

    append l n = l ++ n;

    member x [] = False;
    member x (h:t) = x == h || member x t;

    reverse1 [] = [];
    reverse1 l@(h:t) = foldl (flip (:)) [] l;
    
    last1 [] = error "empty list has no last elem";
    last1 (x:[]) = x;
    last1 (_:t) = last1 t;

    filter1 p l = [n | n <- l, p n];

    map1 f l =[f(n) | n <-l];


    mergesort [] = []; 
    mergesort (x:[]) = [x];
    mergesort l = merge (mergesort (fst(spl2 l)), mergesort (snd(spl2 l))) where
        spl2 l = splitAt ((length l) `div` 2 ) l
        merge (a, []) = a
        merge ([], b) = b
        merge ((a:at),(b:bt)) 
            | a<b = a : merge (at, (b:bt))
            | otherwise = b : merge (bt, (a:at))
    ;

    insertionsort [] = [];
    insertionsort (h:[]) = [h];
    insertionsort (h:g)
        | h < (head g) = insertionsort (h : (insertionsort g))
        | otherwise = insertionsort ((head g) : (insertionsort (h : (tail g))))
    ;


    lfold f n [] = n;
    lfold f n (x:xs) = lfold f (f n x) xs;


    rfold f n [] = n;
    rfold f n (x:xs) = f x (rfold f n xs); 

    len = foldl (\x y -> x+1) 0;

    and = foldl (\x y -> x && y ) True;

    prod = foldl (*) 1;

    nwd l = foldl gcd (prod l) l;

    del el = foldr f [] where
        f x y
            | x == el = y
            | otherwise = x:y
    ;

    my_map g = foldr (\x y -> g(x):y ) [] ;
}
