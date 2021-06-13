/* Examples for testing */

timesfloat -2.0 3.14159;

let x=true in x;

let x=plus -3 6 in gt x 5;

plus -3.0 2.1;

[1,2,3];

[1.0];

[2.1, 2.3, 3.5, 8.7];

head [1,2,3];

tail [2.1, 2.3, 3.5, 8.7];

isnil [1.0];

isnil [] as List Int;

let x=[1,2,3] in tail x;

cons 1 [2,5,8];

let x=[1,2] in cons 3 (cons 4 x);

y = [1.0, 2.1];

cons -0.5 y;

cons 1.0 + 0.5 y;



(lambda x:List Float. head x) [2.3,3.3]; 

[1+2, 3*4, 5];

1@1 + 2@3;

10.0 @ 3;

z=[1@1, 2@2];

head z;

[1 + 2@3, 4@2 * 5,6@3 - 7@3];

/*
map=lambda g:Float->Float.
    fix(lambda f:List Float->List Float. lambda l:List Float. 
    if isnil l then [] as List Float else cons (g (head l)) (f (tail l)) ) ;

map ((lambda X<:Float. lambda x:X. x*2.0) [Float])  [2.6@1,4.8@2];
*/
map=lambda X. lambda Y. lambda g:X->Y.
    fix(lambda f:List X->List Y. lambda l:List X. 
    if isnil l then [] as List Y else cons (g (head l)) (f (tail l)) ) ;

map [Float] [Float] ((lambda X<:Float. lambda x:X. x*2.0) [Float])  [2.6@1,4.8@2];

/*
cons 2 (cons 3.2 [] as List Int);

cons (lambda x:Float. x*2.0) [] as List Float;
*/

