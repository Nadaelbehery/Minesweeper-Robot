main = do
   putStrLn "Please Enter Grid size"
   sizeOfTheGrid <- getLine
   let size = read sizeOfTheGrid::Int
   putStrLn ("The grid Size is " ++ show size ++".")
   --num<- random,:: IO Float



type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState  deriving(Show,Eq)
up:: MyState -> MyState
up (S (b,c) m s t )= if b>0  then (S ((b-1),c) m "up" (S (b,c) m s t )) 
                
				
				 else Null 
down:: MyState -> MyState
down (S (b,c) m s t )= if b< 10  then (S ((b+1),c) m "down" (S (b,c) m s t)  ) 
                
				
				 else Null
left:: MyState -> MyState
left (S (b,c) m s t )= if c>0  then (S (b,(c-1)) m "left" (S (b,c) m s t ) ) 
                
				
				 else Null	
right:: MyState -> MyState
right (S (b,c) m s t)= if c< 10  then (S (b,(c+1)) m "right" (S (b,c) m s t ) ) 
                
				
				 else Null

				 
collect:: MyState -> MyState
areTheySame1 :: Cell -> Cell-> [Cell]

areTheySame1 (a,b) (c,d) | (a == c && b==d) = []
                | otherwise = [(c,d)]

removeItem1 :: Cell -> [Cell] -> [Cell]
removeItem1 a []=[]

removeItem1 (a,b) ((c,d):ys) = areTheySame1 (a,b) (c,d)  ++ removeItem1 (a,b) ys

collect (S (b,c) m s t1 ) =
                        if(elem (b,c) m)
						      
							  then (S (b,c) (removeItem1 (b,c) m ) "collect" (S (b,c) m s t1))
							  else  Null
						

{-collect (S (b,c) m s t )|(length m)==0=Null 
                       | (length m)==1 && elem (b,c) m  = (S (b,c) [] "collect" (S (b,c) m s t))			 
				       | (length m)==2 && elem (b,c) m && (m!!1)==(b,c) = (S (b,c) (init m)  "collect" (S (b,c) m s t))
					   |(length m)==2 && elem (b,c) m && (m!!0)==(b,c) = (S (b,c) [m!!1]  "collect" (S (b,c) m s t))
					   | otherwise=Null-}
							
							
					   
nextMyStates::MyState->[MyState]

nextMyStates v = removeItem ([up v] ++ [down v] ++ [left v] ++ [right v] ++ [collect v])

areTheySame ::  MyState-> [MyState]

areTheySame  y | y == Null = []
                | otherwise = [y]

removeItem :: [MyState]-> [MyState]
removeItem []=[]

removeItem (y:ys) = areTheySame y ++ removeItem ys
isGoal::MyState->Bool
isGoal(S (b,c) m s t)=if (length m)==0 then True
                      else False

 

				
search::[MyState]->MyState
search []=Null
search (h:t)=if isGoal h then h
             else search (t ++ nextMyStates h)   
constructSolution:: MyState ->[String]
constructSolution (S (b,c) m "" t) = []
constructSolution (S (b,c) m s t)=constructSolution t ++ [s]
solve :: Cell->[Cell]->[String]
solve c l =constructSolution (search (nextMyStates (S c l "" Null)))

  