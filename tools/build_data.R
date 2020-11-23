# build_data.R

logical_token <- c("AND", "AND2", "OR", "OR2", "GT", "LT", "LE", "GE", "NE", "EQ" )
math_token <- c( "'+'", "'-'", "'*'", "'/'", "'^'" )
special_token <- c( "SPECIAL-PIPE", "SPECIAL-IN", "SPECIAL-OTHER" )
op_token <- c( "SPECIAL-PIPE", "SPECIAL-IN", "SPECIAL-OTHER", "AND", "AND2"
             , "OR", "OR2", "GT", "LT", "LE", "GE", "NE", "EQ", "EQ_SUB"
             , "EQ_ASSIGN", "LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_SUB", "ELSE"
             , "IN", "EQ_FORMALS"
             )

usethis::use_data( logical_token
                 , math_token
                 , special_token
                 , op_token
                 , internal = TRUE
                 , overwrite = TRUE
                 )
