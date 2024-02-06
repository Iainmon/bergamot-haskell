
termParser :: Parser (Term UnificationVar)
termParser = do
  name <- identifier
  args <- option [] (parens (termParser `sepBy` comma))
  return (Term name args)

ruleParser :: Parser (Rule UnificationVar)
ruleParser = do
  name <- identifier
  symbol "@"
  body <- option [] (angleBrackets (termParser `sepBy` comma))
  symbol "<-"
  return (Rule name body)
