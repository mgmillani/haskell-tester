module Main
	where

import Tester

inputs = [(1,1) ,(3 , 8), (2,4) , (4 , 10)]
convert (inp,out,right) = (show inp, show out, show right)

inputs2 = [((1,1),2) , ((3,2),4) , ((3,4),7) , ((1,3),5)]

convert2 ((x,y),out,right) = ((show x) ++" , " ++(show y), show out, show right)
add x y = x+y
main = do
	test "Square" (\x -> x*x) (\x y -> x==y) convert inputs
	test "Add" (\(x,y) -> add x y) (\x y -> x==y) convert2 inputs2

