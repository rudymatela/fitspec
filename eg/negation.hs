-- Minimal example: mutation testing a negation specification
import FitSpec

properties not =
  [ property $ \p -> not (not p) == p
  , property $ \p -> not (not (not p)) == not p
  ]

main = mainWith args {names = ["not p"]} not properties
