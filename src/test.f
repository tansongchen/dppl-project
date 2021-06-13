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

let x=[1,2] in cons 3 x;

y = [1.0, 2.1];

cons -0.5 y;

(lambda x:List Float. x) [] as List Float;

cons 1.0 + 0.5 y;

[1+2, 3*4, 5];