# test_setupstyle.R

library(testthat)
library(styler)
library(commastyler)

test_initialize <- function( pd_flat ) {
  init_pd <- ( initialize_newlines( pd_flat )
             %>% styler:::initialize_spaces()
             %>% styler:::remove_attributes( c( "line1", "line2", "col1", "col2"
                                              , "parent", "id"
                                              )
                                           )
             %>% styler:::initialize_multi_line()
             %>% styler:::initialize_indention_ref_pos_id()
             %>% styler:::initialize_indent()
             %>% styler:::validate_parse_data()
             )
  init_pd
}

test_that( "set_line_break_around_comma", {
  code <-
    c( "add_xy <- function(x,"
     , "y) { # comment"
     , "x+c(y,1)}"
     )
  set_line_break_before_curly_opening_style <- function() {
    create_style_guide(line_break = tibble::lst( commastyler:::set_line_break_around_comma )
                      , style_guide_name = "set_line_break_before_curly_opening_style"
                      , style_guide_version = packageVersion("commastyler")
                      )
  }
  #debug(set_line_break_around_comma)
  result <- style_text( code
                      , style = set_line_break_before_curly_opening_style
                      )

  expect_equal( result
              , structure( c( "add_xy <- function(x"
                            , ",y"
                            , ") { # comment", "x+c(y"
                            , ",1"
                            , ")}"
                            )
                         , class = "vertical"
                         )
              )

})

# test_that( "set_line_break_around_comma", {
#   code <-
#     c( "add_xy <- function(x,"
#      , "y) { # comment"
#      , "x+c(y,1)}"
#      )
#   test_style <- function() {
#     create_style_guide( initialize = tibble::lst( test_initialize )
#                       , line_break = tibble::lst( commastyler:::set_line_break_around_comma )
#                       , space = tibble::lst( commastyler_reindention = commastyler::commastyler_reindention )
#                       , token = tibble::lst( force_assignment_op = commastyler:::force_assignment_op )
#                       , indention = tibble::lst( update_indention_ref_fun_dec = commastyler:::update_indention_ref_fun_dec )
#                       , style_guide_name = "test_style"
#                       , style_guide_version = packageVersion("commastyler")
#                       )
#   }
#   result <- style_text( code
#                       , style = test_style
#                       )
# })

