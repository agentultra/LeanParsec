structure Parser (α : Type u) where
  runParser : ByteArray → OptionM (ByteArray × α)

namespace Parser

def map {α β : Type u} (f : α → β) (p : Parser α) : Parser β :=
  Parser.mk $ fun input => do
    let (input', p') ← p.runParser input
    return (input', f p')

instance : Functor Parser where
  map := Parser.map

end Parser
