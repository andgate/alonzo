module Language.Alonzo.Transform.Lifted where


data Lambda = Lambda Text [Text] Term

data Term
  = TVal
  | TPrim PrimInstr Term Term
  | TApp Term [Term]
  | TLet (Bind (Rec [(Var, Embed Term)]) Term)
  | TLoc Loc Term
  | TWild
  deriving(Show, Generic, Typeable)