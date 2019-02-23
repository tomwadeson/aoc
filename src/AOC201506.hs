{-# LANGUAGE LambdaCase #-}

module AOC201506 where

import qualified Data.Map.Strict               as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Void                      ( Void )
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( foldl' )

type Parser = Parsec Void String

newtype Point =
    Point (Int, Int) deriving (Show, Eq, Ord)

newtype Range =
    Range (Point, Point) deriving (Show)

points :: Range -> [Point]
points (Range (Point (x1, y1), Point (x2, y2))) =
    [ Point (x, y) | x <- [x1 .. x2], y <- [y1 .. y2] ]

data Instruction = TurnOn Range
                 | TurnOff Range
                 | Toggle Range deriving (Show)

newtype Grid a =
    Grid (M.Map Point a) deriving (Show)

mkGrid :: Range -> a -> Grid a
mkGrid r a = Grid $ foldl' (\acc x -> M.insert x a acc) M.empty (points r)

parsePoint :: Parser Point
parsePoint = do
    x <- L.decimal
    single ','
    y <- L.decimal
    pure $ Point (x, y)

parseRange :: Parser Range
parseRange = do
    f <- parsePoint
    string " through "
    t <- parsePoint
    pure $ Range (f, t)

parseInstruction :: Parser Instruction
parseInstruction = action <*> parseRange
  where
    action  = turnOn <|> turnOff <|> toggle
    turnOn  = TurnOn <$ string "turn on "
    turnOff = TurnOff <$ string "turn off "
    toggle  = Toggle <$ string "toggle "

parseInstructions :: Parser [Instruction]
parseInstructions = many (parseInstruction <* sc)
    where sc = L.space space1 empty empty

data InstructionInterpreter a =
    InstructionInterpreter {
        turnOn  :: a -> a,
        turnOff :: a -> a,
        toggle  :: a -> a
    }

update :: Range -> Grid a -> (a -> a) -> Grid a
update r (Grid g) f = Grid $ foldl' (flip (M.adjust f)) g (points r)

follow :: InstructionInterpreter a -> Grid a -> [Instruction] -> Grid a
follow interpreter = foldl'
    (\acc x -> case x of
        (TurnOn  r) -> update r acc (turnOn interpreter)
        (TurnOff r) -> update r acc (turnOff interpreter)
        (Toggle  r) -> update r acc (toggle interpreter)
    )

readInstructions :: String -> [Instruction]
readInstructions input = fromMaybe [] (parseMaybe parseInstructions input)

solution :: a -> InstructionInterpreter a -> (Grid a -> Int) -> String -> Int
solution init interpreter brightness input =
    let initialGrid  = mkGrid (Range (Point (0, 0), Point (999, 999))) init
        instructions = readInstructions input
        grid         = follow interpreter initialGrid instructions
    in  brightness grid

data Light = LightOn
           | LightOff deriving (Show)

partOne :: String -> Int
partOne = solution LightOff interpreter brightness
  where
    interpreter = InstructionInterpreter
        { turnOn  = const LightOn
        , turnOff = const LightOff
        , toggle  = \case
                        LightOn  -> LightOff
                        LightOff -> LightOn
        }
    brightness (Grid g) =
        sum
            . M.map
                  (\case
                      LightOn  -> 1
                      LightOff -> 0
                  )
            $ g

newtype BrightLight =
    BrightLight { getBrightLight :: Int }

partTwo :: String -> Int
partTwo = solution (BrightLight 0) interpreter brightness
  where
    interpreter = InstructionInterpreter
        { turnOn  = BrightLight . (+ 1) . getBrightLight
        , turnOff = BrightLight
                    . (\n -> if n > 0 then n - 1 else 0)
                    . getBrightLight
        , toggle  = BrightLight . (+ 2) . getBrightLight
        }
    brightness (Grid g) = sum . M.map getBrightLight $ g
