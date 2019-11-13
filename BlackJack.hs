module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

hand13 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) 
          --  (Add (Card Ace Spades)
            (Add(Card Ace Hearts) Empty ))--)

hand17 = Add (Card (Numeric 2) Hearts)
            (Add (Card (Numeric 4) Spades) 
          --  (Add (Card Ace Spades)
            (Add(Card Ace Hearts) Empty ))--)

hand21 = Add (Card King Spades)
        (Add(Card Ace Hearts) Empty )

handFat = Add (Card King Spades)
        (Add(Card Queen Hearts) 
        (Add (Card Jack Spades) Empty))

displayCard :: Card -> String
displayCard (Card (Numeric x) s) = show x ++ " of " ++ show s ++  "\n"
displayCard (Card r s)           = show r ++ " of " ++ show s ++  "\n"

display :: Hand -> String
display Empty = ""
display(Add c h) = displayCard(c) ++ display h


numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add c h) | rank c == Ace = 1 + numberOfAces h
                       | otherwise     = numberOfAces h

value :: Hand -> Integer
value Empty = 0
value h | aceValue h 11 <= 21 = aceValue h 11
        | otherwise = aceValue h 1


aceValue :: Hand -> Integer -> Integer
aceValue Empty _ = 0
aceValue(Add (Card (Numeric n) _) h) a = n + aceValue h a
aceValue(Add (Card r _) h) a
  | r == Ace = a + aceValue h a
  | otherwise = 10 + aceValue h a

gameOver :: Hand -> Bool
gameOver Empty = False
gameOver (Add c h) | value (Add c h) > 21 = True
                   | otherwise         = False

winner :: Hand -> Hand -> Player
winner (Add gc gh) (Add bc bh) | gameOver(Add gc gh) = Bank
                               | gameOver (Add bc bh) = Guest
                               | value(Add bc bh) >= value(Add gc gh) = Bank
                               | otherwise         = Guest

 