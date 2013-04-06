{-
Copyright 2013 Marcelo Garlet Millani

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

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
