module Tests (Test(Test), tests) where

type Input = String 
type Output = String
data Test = Test Input Output -- deriving Show
instance Show Test where 
  show (Test input output) = 
    "Input:\n" ++ input ++ "\n" ++
    "\nOutput: " ++ output ++ "\n\n" 

t01 = Test "*"
           "111"

t02 = Test "#  #  *  #  #"
           "311"

t03 = Test "#####\n\
           \#####\n\
           \####*\n\
           \#####\n\
           \#####"
           "135"

t04 = Test "         #####\n\
           \         #####           ######\n\
           \         #####           ######\n\
           \#  #  #  #####  #  #  #  ######  *"
           "911"

t05 = Test "#\n\
           \#  #\n\
           \#  #  ##\n\
           \#  #  ##  ###\n\
           \#  #  ##  ###  #####\n\
           \#  #  ##  ###  ##*##  ########\n\
           \#  #  ##  ###  #####  ########"
           "523"

t06 = Test "           #\n\
           \           *\n\
           \           #\n\
           \           #\n\
           \           #\n\
           \           #\n\
           \           #\n\
           \           #\n\
           \#########  #  #"
           "281"

t07 = Test "                        ########*\n\
           \                        #########\n\
           \                        #########\n\
           \                        #########\n\
           \                        #########\n\
           \                        #########\n\
           \                        #########\n\
           \                        #########\n\
           \#  #  #  #  #  #  #  #  #########"
           "999"

t08 = Test "#  ###  #\n\
           \#  ###  # #*\n\
           \#  ###  ####  #\n\
           \#  ###  #  #  #"
           "333"

t09 = Test "      *\n\
           \      ##\n\
           \     ####\n\
           \    ##  ##\n\
           \   ########\n\
           \  ###    ###\n\
           \ ############\n\  
           \###        ###"
           "181"

tests = [t01,t02,t03,t04,t05,t06,t07,t08,t09]
