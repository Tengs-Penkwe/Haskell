data Pair a b = Pair a b
  deriving(Show)

-- stmt :: Pair String Bool
-- stmt = (,) "This" False

data OneOfThree a b c = Sinistral a | Medial b | Dextral c
  deriving(Show)

data MyMaybe a = Either () a

data List a = Nil | Cons a (List a)
  deriving(Show)
