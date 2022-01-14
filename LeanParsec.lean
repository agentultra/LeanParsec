structure Parser (α : Type u) where
  runParser : String → OptionM (String × α)

namespace Parser

def map {α β : Type u} (f : α → β) (p : Parser α) : Parser β :=
  Parser.mk $ fun input => do
    let (input', p') ← p.runParser input
    return (input', f p')

instance : Functor Parser where
  map := Parser.map

end Parser

def charP (c : Char) : Parser Char :=
  Parser.mk $ λ xs =>
    if String.isPrefixOf (String.mk [c]) xs
    then Option.some (String.drop xs 1, c)
    else Option.none
