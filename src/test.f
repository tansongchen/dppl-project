/* Examples for testing */

x ~ true;

y ~ (-3 + 6);

y > 5;

-3.0 + 2.1;

[1,2,3];

[1.0];

[2.1, 2.3, 3.5, 8.7];

head [1,2,3];

tail [2.1, 2.3, 3.5, 8.7];

isnil [1.0];

isnil Int [];

z ~ [1,2,3];

tail z;

1 :: [2,5,8];

x ~ [1,2];

(3 :: (4 :: x));

y ~ [1.0, 2.1];

-0.5 :: y;

1.0 + 0.5 :: y;

(lambda x:List Float. head x) [2.3,3.3]; 

[1+2, 3*4, 5];

1@1 + 2@3;

10.0 @ 3;

z ~ [1@1, 2@2];

head z;

[1 + 2@3, 4@2 * 5,6@3 - 7@3];

map ~ lambda X. lambda Y. lambda g:X->Y.
    fix(lambda f:List X->List Y. lambda l:List X. 
    if isnil l then Y [] else (g (head l)) :: (f (tail l)) ) ;

map [Float] [Float] (lambda x:Float. x*2.0)  [2.6@1,4.8@2];

[(2*4@2)@3,1@3];

((2@3+3)@2*2)@1;
