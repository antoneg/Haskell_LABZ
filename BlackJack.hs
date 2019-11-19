module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

implementation = Interface
        { iFullDeck = fullDeck
        , iValue    = value
        , iDisplay  = display
        , iGameOver = gameOver
        , iWinner   = winner
        , iDraw     = draw
        , iPlayBank = playBank
        , iShuffle  = shuffleDeck
        }
main :: IO ()
main = runGame implementation

-- AX

hand2 = Add (Card (Numeric 2) Hearts)
    (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            ,2]

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
winner Empty _ = error "Empty hand"
winner _ Empty = error "Empty hand"
winner (Add gc gh) (Add bc bh) | gameOver(Add gc gh) = Bank
                               | gameOver (Add bc bh) = Guest
                               | value(Add bc bh) >= value(Add gc gh) = Bank
                               | otherwise         = Guest

-- BX..

--B1
--Given two hands, <+ puts the first one on top of the second one:
(<+) :: Hand -> Hand -> Hand
e <+ Empty = e
Empty <+ e = e
(Add c h) <+ h2 = (Add c (h <+ h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = 
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool

prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

--B2
fullList :: [(Rank,Suit)]
fullList = [(rank,suit) | rank <-([Numeric n | n <- [2..10]] ++ [Jack,Queen,King,Ace])
                        , suit <- [Hearts, Spades , Diamonds , Clubs]]
fullDeck :: Hand
fullDeck = foldr (<+) Empty deck
  where
    deck = [Add (Card rank suit) Empty | (rank , suit) <- fullList ] 

--B3
-- Given a deck and a hand, draw one card from the deck and put on the hand. Return both the
-- deck and the hand (in that order).
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error"draw: The deck is empty."
draw (Add drawCard restOfDeck) h = (restOfDeck, (Add drawCard Empty) <+ h)

--B4
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

playBankHelper :: Hand -> Hand -> Hand
playBankHelper _ hand
    | gameOver hand = hand
    | value hand > 15 = hand
playBankHelper deck hand = playBankHelper x y
    where (x,y) = (draw deck hand) 

--B5
-- Help function: Given a hand, output a list of all cards contained in the hand. 
cardsList :: Hand -> [Card]
cardsList Empty = []
cardsList (Add c h) = c:cardsList h 

-- Help function: Remove a card from a hand and output the rest of a hand. 
handRemoveCard :: Hand -> Card -> Hand
handRemoveCard Empty _ = Empty
handRemoveCard (Add c h) card | c == card = h  
                          |otherwise = (Add c (handRemoveCard h card))

-- Pick a random card from a deck and add it to a new hand. 
-- Continue adding a random card to the new hand until the deck (which we pick from) is empty. 
-- Please be kind, One Love 
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g Empty = Empty
shuffleDeck g (Add c h) = Add (restOfCards !! n) 
                              (shuffleDeck g1 (handRemoveCard (Add c h) (restOfCards !! n)))
    where
      restOfCards = cardsList((Add c h))
      (n, g1) = randomR (0,size h) g

-- PropTests
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)



