# solving training problems  
   Problems in [here](http://aperiodic.net/phil/scala/s-99/)  

# policy  
   I solve odd one in haskell.  

# memo  
   *  not exist  
      No.29, No.30, No.42-45 does not exist  
   *  p17  
      +  arrow op `&&&` in `Control.Arrow`  
         -  `(&&&) :: a b c -> a b c' -> a b (c, c')`  
         -  e.g.  
            `f &&& g == (\x -> (f x, g x))`  
      +  arrow op `***` in `Control.Arrow`  
         -  `(***) :: a b c -> a b' c' -> a (b, b') (c, c')`  
         -  e.g.  
            `f *** g == (\(x,y) -> (f x, g y))`  
   *  p27  
      should be careful  
      +  correct anser  

             combination :: Int -> [a] -> [([a],[a])]
             combination 0 xs = [([],xs)]
             combination _ [] = []
             combination n (x:xs) = picked ++ rests 
                 where picked = [(x:pa, pb) | (pa,pb) <- combination (n-1) xs]
                       rests  = [(ra, x:rb) | (ra,rb) <- combination n xs]

         `combination 1 "abc"` -> `[("a","bc"),("b","ac"),("c","ab")]`  

      +  wrong answer 1  

             combination :: Int -> [a] -> [([a],[a])]
             combination 0 xs = [([],xs)]
             combination _ [] = [([],[])] -- here is changed
             combination n (x:xs) = picked ++ rests 
                 where picked = [(x:pa, pb) | (pa,pb) <- combination (n-1) xs]
                       rests  = [(ra, x:rb) | (ra,rb) <- combination n xs]

         `combination 1 "abc"` -> `[("a","bc"),("b","ac"),("c","ab"),("","abc")]`  
         this program means not *choose 1*, but *choose less than 1*  

      +  wrong answer 2  

             combination :: Int -> [a] -> [([a],[a])]
             combination _ [] = []        -- here is switched
             combination 0 xs = [([],xs)] -- here is switched
             combination n (x:xs) = picked ++ rests 
                 where picked = [(x:pa, pb) | (pa,pb) <- combination (n-1) xs]
                       rests  = [(ra, x:rb) | (ra,rb) <- combination n xs]

         `combination 1 "abc"` -> `[("a","bc"),("b","ac")]`  
         this program finish too short step.  

   *  p67  
      +  This code needs `do` in line:36.  
         I dont know why need it in here.(line:38 doesnt need `do`)  
         Someone, please tell me why.  


