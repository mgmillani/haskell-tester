module Tester
	where

import Text.Printf

separator = "########################"

checkTests verify [] = (0,0,[])
-- h = (input,output,correct)
checkTests verify (h:rest) =
	let (passes,fails,faillist) = checkTests verify rest in
		if (\(x,y,z) -> verify y z) h then (passes+1,fails,faillist) else (passes,fails+1,h:faillist)

showFailed::[Char] -> [([Char],[Char],[Char])] -> IO()
showFailed _ [] = do
	printf "\n"
showFailed fname ((input,output,correct):failed) = do
	printf "Given  : %s(%s) = %s\n" fname input output
	printf "Correct: %s(%s) = %s\n" fname input correct
	printf "-----\n"
	showFailed fname failed

test:: [Char] -> (a -> b) -> (b -> b -> Bool) -> ( (a,b,b) -> (String,String,String)) -> [(a,b)] -> IO ()
test fname f verify convert inputs = do
	printf "%s\nTesting function %s\n" separator fname
	let	applied = map (\(x,y) -> (x ,f x, y)) inputs
	let	(passed,failed,faillist) = checkTests verify applied
	printf "%d tests passed\n" (passed::Int)
	printf "%d tests failed\n\n" (failed::Int)
	printf "These tests failed:\n\n"
	let printedFail = map convert faillist
	showFailed fname printedFail
	printf "%s\n" separator

	{-

inputs = [(1,1) ,(3 , 8), (2,4) , (4 , 10)]
convert (inp,out,right) = (show inp, show out, show right)

inputs2 = [((1,1),2) , ((3,2),4) , ((3,4),7) , ((1,3),5)]

convert2 ((x,y),out,right) = ((show x) ++" , " ++(show y), show out, show right)
add x y = x+y
main = do
	test "Square" (\x -> x*x) (\x y -> x==y) convert inputs
	test "Add" (\(x,y) -> add x y) (\x y -> x==y) convert2 inputs2
	-}