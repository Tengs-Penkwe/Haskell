type Action = Int
data State = State ([Action],[Action]) [Action]
         deriving (Eq, Show)
data Result = EndOfGame Double State
            | ContinueGame State
         deriving (Eq, Show)

