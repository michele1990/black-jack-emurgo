-- an Haskell version of the well known game BlackJack


import Control.Monad -- --> to use when statement
import Data.Time.Clock.POSIX -- --> to perform shiffling introducing random choice
import System.IO.Unsafe -- --> to perform shiffling introducing random choice


-- LOGIC
-- BUILD A DECK (DATA DECLARATION - PROPER BUILDING)
-- SHUFFLING THE DECK --> USING unsafePerformIO 

-- THE GAME!

-- DEALER HAND GENERATION WITH IO
-- PLAYER HAND GENERATION WITH IO

-- PLAYER HIT OR STAY (BASED ON PLAYER DECISION) WITH IO

--PURE PART

-- DEALER HIT OR STAY (BASED ON PRE-DECIDED CONDITIONS) (hige ruther developlmend could be made on this part)


-- SCORE EVALUATION -- HANDS COMPARISON

    -- EVALUATE CARD VALUE
    -- AVALUATE HAND VALUE (TAKING INTO ACCOUNT ACE 1 OR 11 VALUE)
    -- COMPARE HANDS

-- DECIDE WINNER


-- IO
-- END GAME WITH IO


-----------------------
-----------------------
---- DECK BUIDLING-----
-----------------------
-----------------------


-- DATA DECLARATION

data Suit = Clubs|Diamonds|Hearts|Spades
            deriving (Show, Eq, Ord, Enum, Bounded)

data Value = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
             deriving (Show, Eq, Ord, Enum, Bounded)


data PlayingCards = Card {val::Value,su::Suit}
                    deriving (Show, Eq)



-- DECK BUILDING

allSuits = [(minBound::Suit) ..]
allValues = [(minBound::Value) ..]
buildDeck :: [Value] -> [Suit] -> [PlayingCards]
buildDeck a b = [Card x y | x <- a, y <- b]
-- CALL SHUFFLE FUNCTION
deck = shuffle (buildDeck allValues allSuits)     


-- DECK SHUFFLING

getRandom :: Int -> Int
getRandom n = round (unsafePerformIO getPOSIXTime)  `mod` n

shuffle :: [a] -> [a]
shuffle xs 
    | length xs < 2 = xs
    | otherwise = do
        --let n = getRandom (51)
        let n = getRandom (length xs -1)
        [xs !! n] ++ shuffle (take n xs ++ drop (n+1) xs)




-----------------------
-----------------------
----PLAY BLACKJACK-----
-----------------------
-----------------------

main = do
    blackjack deck


-- THIS FUNCTION HANDLES ALL THE GAMES OPERATIONS

blackjack :: [PlayingCards] -> IO () --This function sets up the dealer's and player's hands, then calls hitOrStay to complete the game
blackjack d = do
    -- DEALER HAND GENERATION
    let dHand = []  -- EMPTY HAND
    let disDHand = dHand ++ (take 2 d) -- TAKE FIRST TWO CARDS FROM THE DECK
    let dTemp = drop 2 d -- UPDATES TEMPORARY DECK DROPPING THE TWO CARD TAKEN
    let d = dTemp -- UPDATES DECK DROPPING THE TWO CARD TAKEN
    let hidDHand = head disDHand -- TAKE HEAD TO SHOW ONLY ONE CARD
    putStrLn ("Dealer's Hand: " ++ show (val hidDHand,su hidDHand) ++ " ****") -- SHOW DEALER HAND
    -- PLAYER HAND GENERATION
    let pHand = [] -- EMPTY HAND
    let disPHand = pHand ++ (take 2 d) -- TAKES FIRST TWO CARD FROM THE DECK
    let dTemp = drop 2 d -- UPDATES TEMPORARY DECK DROPPING THE TWO CARD TAKEN
    let d = dTemp -- UPDATES DECK DROPPING THE TWO CARD TAKEN
    putStrLn ("Your Hand: " ++ show [(val x,su x) | x <- disPHand]) -- SHOW PLAYER HAND
    when (total disPHand == 21) $ do  -- TOTAL EXPLAINED LATER
        dHitOrStay d disDHand disPHand -- IF PLAYER HAND IS 21 MOVE TO DEALER CHOICES
    hitOrStay d disDHand disPHand -- HIR OR STAY TO BE CHOSEN BY PLAYER


-- THIS FUNCTION HANDLES THE PLAYER HIT OR STAY CHOICES OPERATIONS
-- [deck wthout used cards] -> [dealer Hand] -> [player Hand] -> IO
hitOrStay :: [PlayingCards] -> [PlayingCards] -> [PlayingCards] -> IO () 
hitOrStay d disDHand disPHand = do
    putStrLn "Would you like to hit or stay? "
    choice <- getLine
    when (choice /= "hit" && choice /= "stay") $ do -- INPUT ERROR
        putStrLn "Invalid - Please enther 'hit' or 'stay'"
        hitOrStay d disDHand disPHand
    when (choice == "hit") $ do -- PLAYER ASK FOR ANOTHER CARD
        let pHTemp = disPHand ++ (take 1 d) -- TAKE THE FIRST CARD FROM THE DECK
        let dTemp = drop 1 d -- UPDATES TEMPORARY DECK DROPPING THE CARD TAKEN
        let disPHand = pHTemp -- UPDATE PLAYER HAND
        let d = dTemp -- UPDATES DECK DROPPING THE CARD TAKEN
        putStrLn ("Your Hand: " ++ (show [(val x, su x) | x <- disPHand])) -- SHOW PLAYER HAND
        when (total disPHand <= 21) $ do -- IF HAND < 21 UP TO PLAYER TO STAY OR HIT
            hitOrStay d disDHand disPHand -- CALL BACK TO HITORSTAY FUNCTION
        when (total disPHand > 21) $ do -- IF HAND < 21 PLAYER BUSTED 
            endgame "1" disDHand disPHand -- GO TO END GAME -- EXPLAINED LATER
    when (choice == "stay") $ do -- ID PLAYER CHOICES STAY -- IT'T THE DEALER TURN
        putStrLn "The dealer will go now"
        dHitOrStay d disDHand disPHand -- CALL THE DEALER HITORSTAY FUNCTION



-- THIS FUNCTION HANDLES THE PLAYER HIT OR STAY CHOICES OPERATIONS
-- [deck wthout used cards] -> [dealer Hand] -> [player Hand] -> IO
dHitOrStay :: [PlayingCards] -> [PlayingCards] -> [PlayingCards] -> IO () 
dHitOrStay d disDHand disPHand= do
    when (total disDHand >= 17 && total disDHand <= 21) $ do -- DEALER STAY
        findWinner disDHand disPHand
    when (total disDHand > 21) $ do -- DEALER BOOSTED
        endgame "2" disDHand disPHand
    when (total disDHand < 17) $ do -- DEALER HIT
        let dHTemp = disDHand ++ (take 1 d) -- TAKE THE FIRST CARD FROM THE DECK AND APPEND TO ACTUAL DEALER HAND
        let dTemp = drop 1 d -- UPDATES TEMPORARY DECK DROPPING THE CARD TAKEN
        let disDHand = dHTemp -- UPDATE DEALER HAND
        let d = dTemp -- UPDATES DECK DROPPING THE CARD TAKEN
        dHitOrStay d disDHand disPHand --CALL BACK THE BEGINNING OF THE FUNCTION


-----------------------
-----------------------
--- SCORE EVALUATION --
---------  & ----------
----- FIND WINNER -----
-----------------------
-----------------------

findWinner :: [PlayingCards] -> [PlayingCards] -> IO () --WHO IS THE WINNER????
findWinner disDHand disPHand = do
    when (total disDHand > total disPHand) $ do 
        endgame "3" disDHand disPHand 
    when (total disDHand < total disPHand) $ do
        endgame "2" disDHand disPHand
    when (total disDHand == total disPHand) $ do
        endgame "4" disDHand disPHand



valToInt :: PlayingCards -> Int --convert card value to a number value
valToInt c --card
    |val c == Two   = 2
    |val c == Three = 3
    |val c == Four  = 4
    |val c == Five  = 5
    |val c == Six   = 6
    |val c == Seven = 7
    |val c == Eight = 8
    |val c == Nine  = 9
    |val c == Ten   = 10
    |val c == Jack  = 10
    |val c == Queen = 10
    |val c == King  = 10
    |val c == Ace   = 1

--CALCULATE THE TOTAL VALUE OF AN HAND
-- TAKING INTO ACCOUNT ACES VALUE (1 OR 11)

total :: [PlayingCards] -> Int 
total d = totalValue (moveAces [valToInt x | x <- d] 0 (acesCounter [valToInt x | x <- d] 0)) 0

-- totalValue (   [cardlist] Intaccumulator   ) traerse a list and sum *** decide if ace is 1 or 11 calling decideace
    --          moveaces ( [Intlist] Intaccumulator Intnumofaces)


-- LIST OV VALUE ->  INDEX (wasn't able to do this with recursion in all step, used index) -> NUMBER OF ACES -> LIST OF VALUE WITH ACES PUT AT THE END
moveAces :: [Int] -> Int -> Int -> [Int] --move aces to end of hand 
moveAces v w x --LIST OF CARD VAL, POINTING INDEX, NUMBER OF ACES
    |(w == ((length v) - x))        = v
    |((v!!w == 1) || (v!!w == 11)) = moveAces ((take w v) ++ (drop (w + 1) v) ++ [v!!w]) w x
    |((v!!w /= 1) || (v!!w == 11)) = moveAces v (w + 1) x

-- LIST OF VALUE MODIFIED WITH ACES AT THE END -> ACCUMULATOR ->  VALUE OF THE HAND
totalValue :: [Int] -> Int -> Int 
totalValue v t --LIST OF CARD VAL, TOTAL COUNTER
    |(length v == 0)                  = 0
    |(length v == 1) && (head v == 1) = decideAce t
    |(length v == 1) && (head v /= 1) = t + head v
    |otherwise                        = totalValue (tail v) (t + head v)
 
-- DETERMINES WHETER AN ACE SHOULD BE 1 OR 11 
decideAce :: Int -> Int 
decideAce t --TOTAL
    |t >= 11 = t + 1
    |t < 11  = t + 11

-- LIST OV VALUE -> ACCUMULATOR -> OUTPUT OF NUMBER OF ACES
acesCounter :: [Int] -> Int -> Int 
acesCounter v w 
    |(take 1 v) == []                        = w
    |(take 1 v) == [1] || (take 1 v) == [11] = acesCounter (drop 1 v) (w + 1)
    |(take 1 v) /= [1] || (take 1 v) == [11] = acesCounter (drop 1 v) w






-- EXPLORE COMPARISON BETWEEN DELER AND PLAYER HANDS TO DETERMINE WHO WINS
endgame :: String -> [PlayingCards] -> [PlayingCards] -> IO ()  
endgame c disDHand disPHand = do
    putStrLn ("Dealer's Hand: " ++ show [(val x,su x) | x <- disDHand])
    putStrLn ("Your Hand: " ++ show [(val x,su x) | x <- disPHand])
    when (c == "1") $ do
        putStrLn "You busted!"
    when (c == "2") $ do
        putStrLn "You won!"
    when (c == "3") $ do
        putStrLn "The dealer won!"
    when (c == "4") $ do
        putStrLn "You tied!"
    putStrLn "Thank you for playing!"


