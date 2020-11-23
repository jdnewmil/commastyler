# setupstyle.R


set_line_break_before_curly_opening <- function(pd_flat) {
  op <- pd_flat$token %in% "'{'"
  pd_flat$lag_newlines[op] <- 1L
  pd_flat
}

set_line_break_before_comma <- function( pd
                                       , except_token_after = NULL
                                       , except_text_before = NULL
                                       ) {
  if ( !is_function_call( pd ) && !is_subset_expr( pd ) ) {
    return( pd )
  }
  npd <- nrow( pd )
  seq_x <- rlang::seq2( 3L, npd - 1L )
  is_multi_line <- any( ( pd$lag_newlines[ seq_x ] > 0 )
                      | ( pd$token[ seq_x ] == "COMMENT" )
                      )
  if ( !is_multi_line ) {
    return( pd )
  }
  break_pos <- find_line_break_position_in_multiline_call( pd )
  exception_pos <- c( which( pd$token %in% except_token_after )
                    , if_else( pd$child[[1]]$text[1] %in% except_text_before
                             , break_pos
                             , NA
                             )
                    )
  pd$lag_newlines[ setdiff( break_pos, exception_pos ) ] <- 1L
  pd
}

style_line_break_around_curly <- function( strict, pd ) {
  if ( is_curly_expr( pd ) && nrow( pd ) > 2 ) {
    closing_before <- pd$token == "'}'"
    opening_before <- ( ( pd$token == "'{'")
                      & ( pd$token_after != "COMMENT" )
                      )
    to_break <- lag( opening_before, default = FALSE ) | closing_before
    len_to_break <- sum( to_break )
    pd$lag_newlines[ to_break ] <- ifelse( rep( strict
                                              , len_to_break
                                              )
                                         , 1L
                                         , pmax( 1L
                                               , pd$lag_newlines[ to_break ]
                                               )
                                         )
  }
  pd
}

set_line_break_around_curly_curly <- function( pd ) {
  if ( is_curly_expr( pd ) ) {
    opening_before <- ( ( pd$token == "'{'" )
                      & ( pd$token_before == "'{'"
                        | pd$token_after == "'{'"
                        )
                      )
    closing_before <- ( ( pd$token == "'}'" )
                      & ( pd$token_after == "'}'"
                        | pd$token_before == "'}'"
                        )
                      )
    if ( any( opening_before ) && any( closing_before ) ) {
      pd$lag_newlines[ lag( opening_before, default = FALSE ) ] <- 0L
      pd$lag_newlines[ closing_before ] <- 0L
    }
  }
  pd
}

set_line_break_after_opening_if_call_is_multi_line <- function( pd
                                                              , except_token_after = NULL
                                                              , except_text_before = NULL
                                                              ) {
  if ( !is_function_call( pd ) && !is_subset_expr( pd ) ) {
    return( pd )
  }
  npd <- nrow( pd )
  seq_x <- seq2( 3L, npd - 1L )
  is_multi_line <- ( any( ( pd$lag_newlines[ seq_x ] > 0 )
                        | ( pd$token[seq_x] == "COMMENT" )
                        )
                   )
  if ( !is_multi_line ) {
    return( pd )
  }
  break_pos <- find_line_break_position_in_multiline_call( pd )
  exception_pos <- c( which( pd$token %in% except_token_after )
                    , ifelse( pd$child[[1]]$text[1] %in% except_text_before
                            , break_pos
                            , NA
                            )
                    )
  pd$lag_newlines[ setdiff( break_pos, exception_pos ) ] <- 1L
  pd
}

add_brackets_in_pipe_one <- function( pd, pos ) {
  next_non_comment <- next_non_comment( pd, pos )
  rh_child <- pd$child[[ next_non_comment ]]
  if ( nrow( rh_child ) < 2 && rh_child$token == "SYMBOL" ) {
    child <- pd$child[[ next_non_comment ]]
    new_pos_ids <- create_pos_ids( child
                                 , 1
                                 , after = TRUE
                                 , n = 2
                                 )
    new_pd <- create_tokens( tokens = c( "'('", "')'" )
                           , texts = c( "(", ")" )
                           , pos_ids = new_pos_ids
                           , lag_newlines = rep( 0, 2 )
                           )
    pd$child[[ next_non_comment ]] <- bind_rows( pd$child[[ next_non_comment ]]
                                               , new_pd
                                               ) %>% arrange_pos_id()
  }
  pd
}

add_brackets_in_pipe <- function( pd ) {
  is_pipe <- pd$token == "SPECIAL-PIPE"
  Reduce( add_brackets_in_pipe_one, which( is_pipe ), init = pd )
}

resolve_semicolon <- function( pd ) {
  is_semicolon <- pd$token == "';'"
  if ( !any( is_semicolon ) ) {
    return( pd )
  }
  pd$lag_newlines[ lag( is_semicolon ) ] <- 1L
  pd <- pd[ !is_semicolon, ]
  pd
}

add_line_break_after_pipe <- function( pd ) {
  is_pipe <- ( pd$token == "SPECIAL-PIPE"
             & pd$token_after != "COMMENT"
             )
  if (  sum( is_pipe ) > 1
     && !( next_terminal( pd
                        , vars = "token_before")$token_before
                            %in%  c( "'('", "EQ_SUB", "','" )
         )
     ) {
    pd$lag_newlines[ lag( is_pipe ) ] <- 1L
  }
  pd
}

add_space_after_for_if_while <- function( pd_flat ) {
  comma_after <- pd_flat$token %in% c( "FOR", "IF", "WHILE" )
  if ( !any( comma_after ) ) {
    return( pd_flat )
  }
  idx <- comma_after & ( pd_flat$newlines == 0L )
  pd_flat$spaces[ idx ] <- pmax( pd_flat$spaces[ idx ], 1L )
  pd_flat
}

add_space_before_brace <- function( pd_flat ) {
  op_after <- pd_flat$token %in% "'{'"
  if ( !any( op_after ) ) {
    return( pd_flat )
  }
  op_before <- lead( op_after, default = FALSE )
  idx_before <- ( op_before
                & ( pd_flat$newlines == 0L )
                & pd_flat$token != "'('"
                )
  pd_flat$spaces[ idx_before ] <- pmax( pd_flat$spaces[ idx_before ]
                                      , 1L
                                      )
  pd_flat
}

add_space_before_comments <- function( pd_flat ) {
  comment_after <- ( ( pd_flat$token == "COMMENT" )
                   & ( pd_flat$lag_newlines == 0L )
                   )
  if ( !any( comment_after ) ) {
    return( pd_flat )
  }
  comment_before <- lead( comment_after, default = FALSE )
  pd_flat$spaces[ comment_before
                & ( pd_flat$newlines == 0L )
                ] <- pmax( pd_flat$spaces[ comment_before ]
                         , 1L
                         )
  pd_flat
}

character_to_ordered <- function( x, levels, name = substitute( x ) ) {
  if ( !all( ( x %in% levels ) ) ) {
    stop( paste( "all values in"
               , name
               , "must be one of the following:"
               , paste( levels, collapse = ", " )
               )
        )
  }
  factor( x, levels = levels, ordered = TRUE )
}

arrange_pos_id <- function( data ) {
  pos_id <- data$pos_id
  if ( is.unsorted( pos_id ) ) {
    data <- data[ order( pos_id ), , drop = FALSE ]
  }
  data
}

create_pos_ids <- function( pd, pos, by = 0.1, after = FALSE, n = 1 ) {
  direction <- ifelse( after, 1L, -1L )
  first <- find_start_pos_id( pd, pos, by, direction, after )
  new_ids <- seq( first
                , to = first + direction * (n - 1) * by
                , by = by * direction
                )
  validate_new_pos_ids( new_ids, after )
  new_ids
}

create_tokens <- function( tokens
                         , texts
                         , lag_newlines = 0
                         , spaces = 0
                         , pos_ids
                         , token_before = NA
                         , token_after = NA
                         , indention_ref_pos_ids = NA
                         , indents = 0
                         , terminal = TRUE
                         , child = NULL
                         , stylerignore = FALSE
                         , block = NA
                         , is_cached = FALSE
                         ) {
  len_text <- length( texts )
  new_tibble( list( token = tokens
                  , text = texts
                  , short = substr( texts, 1, 5 )
                  , lag_newlines = lag_newlines
                  , newlines = lead( lag_newlines )
                  , pos_id = pos_ids
                  , token_before = token_before
                  , token_after = token_after
                  , terminal = rep( terminal, len_text )
                  , internal = rep( FALSE, len_text )
                  , spaces = spaces
                  , multi_line = rep( FALSE, len_text )
                  , indention_ref_pos_id = indention_ref_pos_ids
                  , indent = indents
                  , child = rep( list( child ), len_text )
                  , stylerignore = stylerignore
                  , block = block
                  , is_cached = is_cached
                  )
            , nrow = len_text
            )
}

find_line_break_position_in_multiline_call <- function( pd ) {
  candidate <- ( which( pd$token == "EQ_SUB" ) - 1L )[ 1 ]
  ifelse( is.na( candidate ) , 3L, candidate )
}

find_start_pos_id <- function( pd
                             , pos
                             , by
                             , direction
                             , after
                             , candidates = NULL
                             ) {
  candidates <- append( candidates, pd$pos_id[ pos ] )
  if ( is.null( pd$child[[ pos ]] ) ) {
    ifelse( after
          , max( candidates )
          , min( candidates )
          ) + by * direction
  }
  else {
    find_start_pos_id( pd$child[[ pos ]]
                     , ifelse( after
                             , nrow( pd$child[[ pos ]] )
                             , 1L
                             )
                     , by
                     , direction
                     , after
                     , candidates
                     )
  }
}

fix_quotes <- function( pd_flat ) {
  str_const <- which( pd_flat$token == "STR_CONST" )
  if ( is_empty( str_const ) ) {
    return( pd_flat )
  }
  pd_flat$text[ str_const ] <- map( pd_flat$text[ str_const ]
                                  , fix_quotes_one
                                  )
  pd_flat
}

fix_quotes_one <- function( x ) {
  rx <- "^'([^\"]*)'$"
  i <- grep( rx, x )
  if ( is_empty( i ) ) {
    return( x )
  }
  xi <- gsub( rx, "\"\\1\"", x[ i ] )
  x[ i ] <- gsub( "\\\\(')|(\\\\[^'])", "\\1\\2", xi )
  x
}

force_assignment_op <- function( pd ) {
  to_replace <- pd$token == "EQ_ASSIGN"
  pd$token[ to_replace ] <- "LEFT_ASSIGN"
  pd$text[ to_replace ] <- "<-"
  pd
}

indent_braces <- function( pd, indent_by ) {
  indent_indices <- compute_indent_indices( pd
                                          , token_opening = c( "'('"
                                                             , "'['"
                                                             , "'{'"
                                                             )
                                          , token_closing = c( "')'"
                                                             , "']'"
                                                             , "'}'"
                                                             )
                                          )
  pd$indent[ indent_indices ] <- ( pd$indent[ indent_indices ]
                                 + indent_by
                                 )
  set_unindention_child( pd
                       , token = "')'"
                       , unindent_by = indent_by
                       )
}

indent_eq_sub <- function( pd
                         , indent_by
                         , token = c( "EQ_SUB", "EQ_FORMALS" )
                         ) {
  eq_sub <- which( pd$token %in% token )
  if ( length( eq_sub ) == 0 ) {
    return( pd )
  }
  has_line_break <- which( pd$lag_newlines > 0 )
  indent_indices <- intersect( eq_sub + 1
                             , has_line_break
                             )
  pd$indent[ indent_indices ] <- ( pd$indent[ indent_indices ]
                                 + indent_by
                                 )
  pd
}

compute_indent_indices <- function( pd
                                  , token_opening
                                  , token_closing = NULL
                                  ) {
  npd <- nrow( pd )
  potential_triggers <- which( pd$token %in% token_opening )
  needs_indention <- needs_indention( pd
                                    , potential_triggers
                                    , other_trigger_tokens = c( "EQ_SUB"
                                                              , "EQ_FORMALS"
                                                              )
                                    )
  trigger <- potential_triggers[needs_indention][ 1 ]
  if ( is.na( trigger ) ) {
    return( numeric( 0 ) )
  }
  start <- trigger + 1
  if ( is.null( token_closing ) ) {
    stop <- npd
  } else {
    stop <- last( which( pd$token %in% token_closing )[ needs_indention ] ) -
      1
  }
  seq2( start, stop )
}

indent_op <- function( pd
                     , indent_by
                     , token = c( math_token
                                , logical_token
                                , special_token
                                , "LEFT_ASSIGN"
                                , "EQ_ASSIGN"
                                , "'$'"
                                )
                     ) {
  indent_indices <- compute_indent_indices( pd, token )
  pd$indent[indent_indices] <- ( pd$indent[ indent_indices ]
                               + indent_by
                               )
  pd
}

indent_without_paren <- function( pd, indent_by = 2 ) {
  (   pd
  %>% indent_without_paren_for_while_fun( indent_by )
  %>% indent_without_paren_if_else( indent_by )
  )
}

is_curly_expr <- function( pd ) {
  if ( is.null( pd ) ) {
    return( FALSE )
  }
  pd$token[ 1 ] == "'{'"
}

is_function_call <- function( pd ) {
  if ( is.null( pd ) || is.na( pd$token_before[ 2 ] ) ) {
    return( FALSE )
  }
  pd$token_before[ 2 ] == "SYMBOL_FUNCTION_CALL"
}

is_subset_expr <- function( pd ) {
  if (  is.null( pd )
     || nrow( pd ) == 1
     ) {
    return( FALSE )
  }
  pd$token[ 2 ] == "'['"
}

indent_without_paren_for_while_fun <- function( pd, indent_by ) {
  tokens <- c( "FOR", "WHILE", "FUNCTION" )
  nrow <- nrow( pd )
  if ( !( pd$token[ 1 ] %in% tokens )
     || is_curly_expr( pd$child[[ nrow ]] )
     || pd$newlines[ length( pd$newlines ) - 1 ] == 0
     ) {
    return( pd )
  }
  pd$indent[ nrow ] <- indent_by
  pd
}

indent_without_paren_if_else <- function( pd, indent_by ) {
  expr_after_if <- next_non_comment( pd, which( pd$token == "')'" )[ 1 ] )
  is_if <- pd$token[ 1 ] %in% "IF"
  has_if_without_curly <- (  is_if
                          && pd$child[[ expr_after_if ]]$token[ 1 ] != "'{'"
                          )
  if ( !is_if ) {
    return( pd )
  }
  needs_indention_now <-
    pd$lag_newlines[ next_non_comment( pd
                                     , which( pd$token == "')'" ) )
                   ] > 0
  if ( needs_indention_now ) {
    pd$indent[ expr_after_if ] <- indent_by
  }
  else_idx <- which( pd$token == "ELSE" )
  if ( length( else_idx ) == 0 ) {
    return( pd )
  }
  expr_after_else_idx <- next_non_comment( pd, else_idx )
  has_else_without_curly_or_else_chid <-
    (  any( pd$token == "ELSE" )
    && pd$child[[ expr_after_else_idx ]]$token[ 1 ] != "'{'"
    && pd$child[[ expr_after_else_idx ]]$token[ 1 ] != "IF"
    )
  needs_indention_now <-
    pd$lag_newlines[ next_non_comment( pd
                                     , which( pd$token == "ELSE" ) )
                   ] > 0
  if (  has_else_without_curly_or_else_chid
     && needs_indention_now
     ) {
    pd$indent[ seq( else_idx + 1, nrow( pd ) ) ] <- indent_by
  }
  pd
}

update_indention_ref_fun_dec <- function( pd_nested ) {
  if ( pd_nested$token[ 1 ] == "FUNCTION" ) {
    seq <- seq2( 3, nrow( pd_nested ) - 2 )
    pd_nested$indention_ref_pos_id[ seq ] <- pd_nested$pos_id[ 2 ]
  }
  pd_nested
}

wrap_if_else_while_for_fun_multi_line_in_curly <- function( pd, indent_by = 2 ) {
  key_token <- when( pd
                   , is_cond_expr(.) ~ "')'"
                   , is_while_expr(.) ~ "')'"
                   , is_for_expr(.) ~ "forcond"
                   , is_function_dec(.) ~ "')'"
                   )
  if ( length( key_token ) > 0 ) {
    pd <- wrap_multiline_curly( pd
                              , indent_by
                              , space_after = ifelse( contains_else_expr( pd )
                                                    , 1
                                                    , 0
                                                    )
                              , key_token = key_token
                              )
  }
  if ( is_cond_expr( pd ) ) {
    pd <- wrap_else_multiline_curly( pd, indent_by, space_after = 0 )
  }
  pd
}

contains_else_expr <- function( pd ) {
  any( pd$token == "ELSE" )
}

is_cond_expr <- function( pd ) {
  pd$token[ 1 ] == "IF"
}

is_function_dec <- function( pd ) {
  if ( is.null( pd ) ) {
    return( FALSE )
  }
  pd$token[ 1 ] == "FUNCTION"
}

next_non_comment <- function( pd, pos ) {
  if (  length( pos ) < 1
     || is.na( pos )
     || pos >= nrow( pd )
     ) {
    return( integer( 0 ) )
  }
  candidates <- seq2( pos + 1L, nrow( pd ) )
  if ( all( candidates %in% which( pd$token == "COMMENT" ) ) ) {
    return( integer( 0 ) )
  }
  setdiff( candidates, which( pd$token == "COMMENT" ) )[ 1 ]
}

next_terminal <- function( pd
                         , stack = FALSE
                         , vars = c( "pos_id", "token", "text")
                         , tokens_exclude = c()
                         ) {
  pd$position <- seq2( 1, nrow( pd ) )
  pd <- pd[ !( pd$token %in% tokens_exclude ), ]
  if ( pd$terminal[ 1 ] ) {
    pd[ 1, c( "position", vars ) ]
  } else {
    current <- next_terminal( pd$child[[ 1 ]]
                            , stack = stack
                            , vars = vars
                            , tokens_exclude = tokens_exclude
                            )
    if ( stack ) {
      bind_rows( pd[ 1, c( "position", vars ) ], current )
    } else {
      current
    }
  }
}

remove_line_break_in_empty_fun_call <- function( pd ) {
  if (  is_function_call( pd )
     && nrow( pd ) == 3
     ) {
    pd$lag_newlines[ 3 ] <- 0L
  }
  pd
}

remove_space_after_excl <- function( pd_flat ) {
  excl <- ( ( pd_flat$token == "'!'" )
          & ( pd_flat$token_after != "'!'" )
          & ( pd_flat$newlines == 0L )
          )
  pd_flat$spaces[ excl ] <- 0L
  pd_flat
}

remove_space_after_fun_dec <- function( pd_flat ) {
  fun_after <- ( ( pd_flat$token == "FUNCTION" )
               & ( pd_flat$lag_newlines == 0L )
               )
  pd_flat$spaces[ fun_after ] <- 0L
  pd_flat
}

addone_space_after_opening_paren <- function( pd_flat ) {
  paren_after <- pd_flat$token %in% c( "'('", "'['", "LBB" )
  if ( !any( paren_after ) ) {
    return( pd_flat )
  }
  pd_flat$spaces[ paren_after & ( pd_flat$newlines == 0L ) ] <- 1L
  pd_flat
}

remove_space_after_unary_pm_nested <- function( pd ) {
  if ( any( pd$token[ 1 ] %in% c( "'+'", "'-'" ) ) ) {
    pd$spaces[ 1 ] <- 0L
  }
  pd
}

remove_space_around_colons <- function( pd_flat ) {
  one_two_or_three_col_after <- ( pd_flat$token
                                %in% c( "':'", "NS_GET_INT", "NS_GET" )
                                )
  one_two_or_three_col_before <- lead( one_two_or_three_col_after
                                     , default = FALSE
                                     )
  col_around <- one_two_or_three_col_before | one_two_or_three_col_after
  pd_flat$spaces[ col_around & ( pd_flat$newlines == 0L ) ] <- 0L
  pd_flat
}

remove_space_before_comma <- function( pd_flat ) {
  comma_after <- pd_flat$token == "','"
  if ( !any( comma_after ) ) {
    return( pd_flat )
  }
  comma_before <- lead( comma_after, default = FALSE )
  idx <- comma_before & ( pd_flat$newlines == 0L )
  pd_flat$spaces[ idx ] <- 0L
  pd_flat
}

remove_space_before_dollar <- function( pd_flat ) {
  dollar_after <- ( ( pd_flat$token == "'$'" )
                  & ( pd_flat$lag_newlines == 0L )
                  )
  dollar_before <- lead( dollar_after, default = FALSE )
  pd_flat$spaces[ dollar_before ] <- 0L
  pd_flat
}

remove_space_before_opening_paren <- function( pd_flat ) {
  paren_after <- pd_flat$token == "'('"
  if ( !any( paren_after ) ) {
    return( pd_flat )
  }
  paren_before <- lead( paren_after, default = FALSE )
  pd_flat$spaces[ paren_before & ( pd_flat$newlines == 0L ) ] <- 0L
  pd_flat
}

remove_terminal_token_before_and_after <- function( pd_flat ) {
  pd_flat$token_before <- NULL
  pd_flat$token_after <- NULL
  pd_flat
}

set_line_break_before_closing_call <- function( pd, except_token_before ) {
  if ( !is_function_call( pd ) && !is_subset_expr( pd ) ) {
    return( pd )
  }
  npd <- nrow( pd )
  is_multi_line <- any( pd$lag_newlines[ seq2( 3L, npd - 1L ) ] > 0 )
  if (!is_multi_line) {
    exception <- which( pd$token_before %in% except_token_before )
    pd$lag_newlines[ setdiff( npd, exception ) ] <- 0L
    return( pd )
  }
  pd$lag_newlines[ npd ] <- 1L
  pd
}

set_space_in_curly_curly <- function( pd ) {
  if ( is_curly_expr( pd ) ) {
    after_inner_opening <- ( pd$token == "'{'"
                           & pd$token_before == "'{'"
                           )
    before_inner_closing <- lead( pd$token == "'}'"
                                & pd$token_after == "'}'"
                                )
    is_curly_curly_inner <- (  any( after_inner_opening, na.rm = TRUE )
                            && any( before_inner_closing, na.rm = TRUE )
                            )
    if ( is_curly_curly_inner ) {
      pd$spaces[ after_inner_opening ] <- 1L
      pd$spaces[ before_inner_closing ] <- 1L
    }
    after_outer_opening <- ( pd$token == "'{'"
                           & pd$token_after == "'{'"
                           )
    before_outer_closing <- lead( pd$token == "'}'"
                                & pd$token_before == "'}'"
                                )
    is_curly_curly_outer <- (  any( after_outer_opening, na.rm = TRUE )
                            && any( before_outer_closing, na.rm = TRUE )
                            )
    if ( is_curly_curly_outer ) {
      pd$spaces[ after_outer_opening ] <- 0L
      pd$spaces[ before_outer_closing ] <- 0L
    }
  }
  pd
}

set_unindention_child <- function( pd, token = "')'", unindent_by ) {
  if ( all( pd$indent == 0 ) || all( pd$terminal ) ) {
    return( pd )
  }
  closing <- which( pd$token %in% token )
  if (  length( closing ) == 0
     || pd$lag_newlines[ closing ] > 0
     ) {
    return( pd )
  }
  first_on_last_line <- last( c( 1
                               , which( pd$lag_newlines > 0
                                      | pd$multi_line
                                      )
                               )
                            )
  on_same_line <- seq2( first_on_last_line, closing - 1 )
  cand_ind <- setdiff( on_same_line, which( pd$terminal ) )
  if ( length( cand_ind ) < 1 ) {
    return( pd )
  }
  candidates <- pd[ cand_ind, ]
  non_candidates <- pd[ -cand_ind, ]
  candidates$child <- map( candidates$child
                         , unindent_child
                         , unindent_by = abs( pd$indent[ closing ]
                                            - pd$indent[ closing - 1 ]
                                            )
                         )
  bind_rows( candidates, non_candidates ) %>% arrange_pos_id()
}

set_linebreak_after_ggplot2_plus <- function( pd ) {
  is_plus_raw <- pd$token == "'+'"
  if ( any( is_plus_raw ) ) {
    first_plus <- which( is_plus_raw )[ 1 ]
    next_non_comment <- next_non_comment( pd, first_plus )
    is_plus_or_comment_after_plus_before_fun_call <- ( lag( is_plus_raw
                                                          ,   next_non_comment
                                                            - first_plus
                                                            - 1
                                                          , default = FALSE
                                                          )
                                                     & ( pd$token_after == "SYMBOL_FUNCTION_CALL"
                                                       | pd$token_after == "SYMBOL_PACKAGE"
                                                       )
                                                     )
    if ( any( is_plus_or_comment_after_plus_before_fun_call ) ) {
      gg_call <- pd$child[[ previous_non_comment( pd, first_plus ) ]]$child[[ 1 ]]
      if (  !is.null( gg_call )
         && isTRUE(  gg_call$text[ gg_call$token == "SYMBOL_FUNCTION_CALL" ]
                  == "ggplot"
                  )
         ) {
        plus_without_comment_after <- setdiff( which( is_plus_raw )
                                             , which( lead( pd$token == "COMMENT" ) )
                                             )
        pd$lag_newlines[ plus_without_comment_after + 1 ] <- 1L
      }
    }
  }
  pd
}

set_space_after_bang_bang <- function( pd_flat ) {
  last_bang <- ( ( pd_flat$token == "'!'" )
               & ( pd_flat$token_after != "'!'" )
               & ( pd_flat$newlines == 0L )
               & ( pd_flat$token_before == "'!'" )
               )
  pd_flat$spaces[ last_bang ] <- 0L
  pd_flat
}

set_space_around_op <- function( pd_flat, strict ) {
  pd_flat <- add_space_after_comma( pd_flat )
  op_after <- pd_flat$token %in% op_token
  op_before <- lead( op_after, default = FALSE )
  op_after <- op_after | pd_flat$token == "','"
  if ( !any( op_after ) ) {
    return( pd_flat )
  }
  if (  sum( pd_flat$lag_newlines ) > 2
     && is_function_call( pd_flat )
     && any( pd_flat$token %in% c( "EQ_SUB", "','" ) )
     ) {
    is_on_aligned_line <- token_is_on_aligned_line( pd_flat )
  } else {
    is_on_aligned_line <- FALSE
  }
  must_have_space_before <- ( op_before
                            & ( pd_flat$newlines == 0L )
                            & !is_on_aligned_line
                            )
  pd_flat$spaces[ must_have_space_before ] <- if (strict) 1L else {
    pmax( pd_flat$spaces[ must_have_space_before ], 1L )
  }
  must_have_space_after <- ( op_after
                           & ( pd_flat$newlines == 0L )
                           & !is_on_aligned_line
                           )
  pd_flat$spaces[ must_have_space_after ] <- if (strict) 1L else {
    pmax( pd_flat$spaces[ must_have_space_after ], 1L )
  }
  pd_flat
}

set_space_before_comments <- function( pd_flat ) {
  comment_after <- ( ( pd_flat$token == "COMMENT" )
                   & ( pd_flat$lag_newlines == 0L )
                   )
  if ( !any( comment_after ) ) {
    return( pd_flat )
  }
  comment_before <- lead( comment_after, default = FALSE )
  pd_flat$spaces[ comment_before & ( pd_flat$newlines == 0L ) ] <- 1L
  pd_flat
}

set_space_between_eq_sub_and_comma <- function( pd ) {
  op_before <- which( pd$token == "EQ_SUB"
                    & lead( pd$token == "','" )
                    )
  pd$spaces[ op_before ] <- 1L
  pd
}

add_space_after_comma <- function( pd_flat ) {
  comma_after <- ( ( pd_flat$token == "','" )
                 & ( pd_flat$newlines == 0L )
                 )
  pd_flat$spaces[ comma_after ] <- pmax( pd_flat$spaces[ comma_after ]
                                       , 1L
                                       )
  pd_flat
}

previous_non_comment <- function( pd, pos ) {
  if ( length( pos ) < 1 || is.na( pos ) || pos > nrow( pd ) ) {
    return( integer( 0 ) )
  }
  candidates <- seq2( 1L, pos - 1L )
  if ( all( candidates %in% which( pd$token == "COMMENT" ) ) ) {
    return( integer( 0 ) )
  }
  last( setdiff( candidates
               , which( pd$token == "COMMENT" )
               )
      )
}

set_space_between_levels <- function( pd_flat ) {
  if ( pd_flat$token[1] %in% c( "FUNCTION", "IF", "WHILE" ) ) {
    index <- ( pd_flat$token == "')'"
             & pd_flat$newlines == 0L
             )
    pd_flat$spaces[ index ] <- 1L
  } else if ( pd_flat$token[ 1 ] == "FOR" ) {
    index <- ( pd_flat$token == "forcond"
             & pd_flat$newlines == 0
             )
    pd_flat$spaces[ index ] <- 1L
  }
  pd_flat
}

start_comments_with_space <- function( pd, force_one = FALSE ) {
  comment_pos <- ( is_comment( pd )
                 & !is_shebang( pd )
                 & !is_code_chunk_header( pd )
                 )
  if ( !any( comment_pos ) ) {
    return( pd )
  }
  comments <- rematch2::re_match( pd$text[ comment_pos ]
                                , "^(?<prefix>#+['\\*]*)(?<space_after_prefix> *)(?<text>.*)$"
                                )
  comments$space_after_prefix <- nchar( comments$space_after_prefix
                                      , type = "width"
                                      )
  comments$space_after_prefix <- set_spaces( spaces_after_prefix = comments$space_after_prefix
                                           , force_one
                                           )
  pd$text[comment_pos] <- paste0( comments$prefix
                                , map_chr( comments$space_after_prefix
                                         , rep_char
                                         , char = " "
                                         )
                                , comments$text
                                ) %>% trimws( "right" )
  pd$short[ comment_pos ] <- substr( pd$text[ comment_pos ], 1, 5 )
  pd
}

style_space_around_math_token <- function( strict, zero, one, pd_flat ) {
  if ( any( pd_flat$token %in% zero ) ) {
    pd_flat <- style_space_around_token( pd_flat
                                       , strict = TRUE
                                       , tokens = zero
                                       , level_before = 0L
                                       , level_after = 0L
                                       )
  }
  if ( any( pd_flat$token %in% one ) ) {
    pd_flat <- style_space_around_token( pd_flat
                                       , strict = strict
                                       , tokens = one
                                       , level_before = 1L
                                       , level_after = 1L
                                       )
  }
  pd_flat
}

style_space_around_tilde <- function( pd_flat, strict ) {
  if ( is_symmetric_tilde_expr( pd_flat ) ) {
    pd_flat <- style_space_around_token( pd_flat
                                       , strict
                                       , "'~'"
                                       , level_before = 1
                                       , level_after = 1
                                       )
  } else if ( is_asymmetric_tilde_expr( pd_flat ) ) {
    pd_flat <- style_space_around_token( pd_flat
                                       , strict = TRUE
                                       , "'~'"
                                       , level_before = 1
                                       , level_after = ifelse( nrow( pd_flat$child[[ 2 ]] ) > 1
                                                             , 1
                                                             , 0
                                                             )
                                       )
  }
  pd_flat
}

token_is_on_aligned_line <- function( pd_flat ) {
  line_idx <- 1 + cumsum( pd_flat$lag_newlines )
  pd_flat$.lag_spaces <- lag( pd_flat$spaces )
  pd_by_line <- split( pd_flat, line_idx )
  last_line_is_closing_brace_only <- 1L == nrow( last( pd_by_line ) )
  relevant_idx <- seq2( 2
                      , ifelse( last_line_is_closing_brace_only
                              , length( pd_by_line ) - 1L
                              , length( pd_by_line )
                              )
                      )
  pd_by_line <- pd_by_line[ relevant_idx ]
  relevant_lag_spaces_col_1 <- map_int( pd_by_line, ~.x$.lag_spaces[ 1 ] )
  col1_is_aligned <- 1L == length( unique( relevant_lag_spaces_col_1 ) )
  if ( !col1_is_aligned ) {
    return( FALSE )
  }
  has_correct_spacing_around_comma <- map_lgl( pd_by_line
                                             , alignment_has_correct_spacing_around_comma
                                             )
  if ( !all( has_correct_spacing_around_comma ) ) {
    return( FALSE )
  }
  has_correct_spacing_around_eq_sub <- map_lgl( pd_by_line
                                              , alignment_has_correct_spacing_around_eq_sub
                                              )
  if ( !all( has_correct_spacing_around_eq_sub ) ) {
    return( FALSE )
  }
  starting_with_comma <- map_lgl( pd_by_line
                                , ~.x$token[ 1L ] == "','"
                                )
  if ( any( starting_with_comma ) ) {
    return( FALSE )
  }
  pd_is_multi_line <- map_lgl( pd_by_line
                             , ~any( .x$multi_line, na.rm = TRUE )
                             )
  if ( any( pd_is_multi_line ) ) {
    return( FALSE )
  }
  pd_by_line <- (   alignment_drop_comments( pd_by_line )
                %>% alignment_ensure_no_closing_brace( last_line_is_closing_brace_only )
                %>% alignment_ensure_trailing_comma()
                )
  pd_by_line <- map( pd_by_line
                   , function( pd_sub ) {
                       pd_sub$lag_newlines <- NULL
                       pd_sub
                     }
                   )
  n_cols <- map_int( pd_by_line
                   , ~sum( .x$token == "','" )
                   )
  start <- ifelse( all( alignment_col1_is_named( pd_by_line ) )
                 , 1
                 , 2
                 )
  for ( column in seq2( start, max( n_cols ) ) ) {
    char_len <- (   alignment_serialize_column( pd_by_line
                                              , column
                                              )
                %>% compact()
                %>% unlist()
                %>% trimws( which = "right" )
                %>% nchar()
                )
    is_aligned <- 1L == length( unique( char_len ) )
    if ( !is_aligned ) {
      return( FALSE )
    }
  }
  TRUE
}

unindent_child <- function( pd
                          , token = c( "')'", "'}'" )
                          , unindent_by = 2
                          ) {
  closing <- which( pd$token %in% token )
  if ( !( "indent" %in% names( pd ) ) ) {
    pd$indent <- 0
  }
  if (  ( length( closing ) > 0 )
     && ( closing == nrow( pd ) )
     ) {
    pd$indent[ closing ] <- pd$indent[ closing ] - unindent_by
  }
  pd
}

unindent_fun_dec <- function( pd ) {
  if ( is_function_dec( pd ) ) {
    idx_closing_brace <- which( pd$token %in% "')'" )
    fun_dec_head <- seq2( 2L, idx_closing_brace )
    pd$indent[ fun_dec_head ] <- 0L
  }
  pd
}

validate_new_pos_ids <- function( new_ids, after ) {
  ref <- ifelse( after
               , floor( new_ids )
               , ceiling( new_ids )
               )
  if ( any( abs( new_ids - ref ) > 0.5 ) )
    stop( "too many ids assigned." )
}

wrap_else_multiline_curly <- function( pd, indent_by = 2, space_after = 0 ) {
  if (  contains_else_expr( pd )
     && pd_is_multi_line( pd )
     && contains_else_expr_that_needs_braces( pd )
     && !any( pd$stylerignore )
     ) {
      else_idx <- which( pd$token == "ELSE" )
      pd$spaces[ else_idx ] <- 1L
      all_to_be_wrapped_ind <- seq2( else_idx + 1L
                                   , nrow( pd )
                                   )
      pd <- wrap_subexpr_in_curly( pd
                                 , all_to_be_wrapped_ind
                                 , indent_by
                                 , space_after
                                 )
  }
  pd
}

wrap_multiline_curly <- function( pd, indent_by, space_after = 1, key_token ) {
  to_be_wrapped_expr_with_child <- next_non_comment( pd
                                                   , which( pd$token == key_token
                                                          )[ 1 ]
                                                   )
  next_terminal <- next_terminal( pd[ to_be_wrapped_expr_with_child
                                    ,
                                    ]
                                )$text
  requires_braces <- (  if_for_while_part_requires_braces( pd
                                                         , key_token
                                                         )
                     && !any( pd$stylerignore )
                     )
  if ( requires_braces
     | next_terminal == "return"
     ) {
      closing_brace_ind <- which( pd$token == key_token )[ 1 ]
      pd$spaces[ closing_brace_ind ] <- 1L
      all_to_be_wrapped_ind <- seq2( closing_brace_ind + 1L
                                   , to_be_wrapped_expr_with_child
                                   )
      pd <- wrap_subexpr_in_curly( pd
                                 , all_to_be_wrapped_ind
                                 , indent_by
                                 , space_after
                                 )
    if ( nrow( pd ) > 5 )
      pd$lag_newlines[ 6 ] <- 0L
  }
  pd
}

alignment_col1_is_named <- function( relevant_pd_by_line ) {
  (   map_lgl( relevant_pd_by_line
             , function( x ) {
                if ( nrow( x ) < 3 ) {
                  return( FALSE )
                }
                (  identical( x$token[ c( 1, 3 ) ]
                            , c( "SYMBOL_SUB", "expr" )
                               )
                && x$token[ 2 ] %in% c( "EQ_SUB", "SPECIAL-IN", "LT"
                                      , "GT", "EQ", "NE"
                                      )
                )
               }
             )
  %>% all()
  )
}

alignment_drop_comments <- function( pd_by_line ) {
  (   map( pd_by_line
         , function( x ) {
            out <- x[ x$token != "COMMENT", ]
            if (nrow(out) < 1) return(NULL)
            else out
           }
         )
  %>% compact()
  )
}

alignment_ensure_no_closing_brace <- function( pd_by_line
                                             , last_line_droped_early
                                             ) {
  if ( last_line_droped_early ) {
    return( pd_by_line )
  }
  last <- last( pd_by_line )
  if ( nrow( last ) == 1 ) {
    pd_by_line[ -length( pd_by_line ) ]
  } else {
    pd_by_line[[ length( pd_by_line ) ]] <- last[ seq2( 1, nrow(last) - 1L), ]
    pd_by_line
  }
}

alignment_ensure_trailing_comma <- function( pd_by_line ) {
  last_pd <- last( pd_by_line )
  last_pd$spaces[ nrow( last_pd ) ] <- 0
  if ( last( last_pd$token ) == "','" ) {
    return( pd_by_line )
  } else {
    pos_id <- create_pos_ids( last_pd
                            , nrow( last_pd )
                            , after = TRUE
                            )
    tokens <- create_tokens( tokens = "','"
                           , texts = ","
                           , lag_newlines = 0
                           , spaces = 0
                           , pos_ids = pos_id
                           ,
                           )
    tokens$.lag_spaces <- 0
    pd_by_line[[ length( pd_by_line ) ]] <- rbind( last_pd, tokens )
    pd_by_line
  }
}

alignment_has_correct_spacing_around_comma <- function( pd_sub ) {
  comma_tokens <- which( pd_sub$token == "','" )
  if ( length( comma_tokens ) == 0 ) {
    return( TRUE )
  }
  relevant_comma_token <- comma_tokens[ seq2( 1
                                            , length( comma_tokens )
                                              - 1L
                                            )
                                      ]
  correct_spaces_before <- pd_sub$.lag_spaces[ relevant_comma_token ] == 0
  correct_spaces_after <- pd_sub$spaces[ relevant_comma_token ] > 0
  (  all( correct_spaces_before )
  && all( correct_spaces_after )
  )
}

alignment_has_correct_spacing_around_eq_sub <- function( pd_sub ) {
  relevant_eq_sub_token <- which( pd_sub$token == "EQ_SUB" )
  if ( length( relevant_eq_sub_token ) == 0 ) {
    return( TRUE )
  }
  correct_spaces_before <- pd_sub$.lag_spaces[ relevant_eq_sub_token ] >= 1
  correct_spaces_after <- pd_sub$spaces[ relevant_eq_sub_token ] >= 1
  (  all( correct_spaces_before )
  && all( correct_spaces_after )
  )
}

alignment_serialize_column <- function( relevant_pd_by_line, column ) {
  map( relevant_pd_by_line
     , alignment_serialize_line
     , column = column
     )
}

contains_else_expr_that_needs_braces <- function( pd ) {
  else_idx <- which( pd$token == "ELSE" )
  if ( length( else_idx ) > 0 ) {
    non_comment_after_else <- next_non_comment( pd
                                              , else_idx
                                              )
    sub_expr <- pd$child[[ non_comment_after_else ]]
    (  !is_cond_expr( sub_expr )
    && !is_curly_expr( sub_expr )
    )
  } else
    FALSE
}

if_for_while_part_requires_braces <- function( pd, key_token ) {
  pos_first_key_token <- which( pd$token == key_token )[ 1 ]
  child <- pd$child[[ next_non_comment( pd, pos_first_key_token ) ]]
  (  pd_is_multi_line( pd )
  && !is_curly_expr( child )
  )
}

is_asymmetric_tilde_expr <- function( pd ) {
  is_tilde_expr( pd, tilde_pos = 1 )
}

is_code_chunk_header <- function( pd ) {
  is_comment <- is_comment( pd )
  is_comment[ is_comment ] <- grepl( "^#[\\+|\\-]"
                                   , pd$text[is_comment]
                                   , perl = TRUE
                                   )
  is_comment
}

is_comment <- function( pd ) {
  if ( is.null( pd ) ) FALSE
  else
    pd$token == "COMMENT"
}

is_shebang <- function( pd ) {
  is_first_comment <- is_comment( pd ) & ( pd$pos_id == 1L )
  is_first_comment[ is_first_comment ] <- grepl( "^#!"
                                               , pd$text[ is_first_comment ]
                                               , perl = TRUE
                                               )
  is_first_comment
}

is_symmetric_tilde_expr <- function( pd ) {
  is_tilde_expr( pd, tilde_pos = 2 )
}

alignment_serialize_line <- function( relevant_pd_by_line, column ) {
  comma_idx <- which( relevant_pd_by_line$token == "','" )
  n_cols <- length( comma_idx )
  if ( column > n_cols ) {
    return( NULL )
  } else {
    relevant_comma <- comma_idx[ column ]
  }
  relevant_pd_by_line <- relevant_pd_by_line[ seq2( 1
                                                  , relevant_comma
                                                  )
                                            , ]
  alignment_serialize( relevant_pd_by_line )
}

is_tilde_expr <- function( pd, tilde_pos = c(1, 2) ) {
  if ( is.null( pd ) || nrow( pd ) == 1 ) {
    return( FALSE )
  }
  any( pd$token[ tilde_pos] == "'~'" )
}

pd_is_multi_line <- function( pd ) {
  any( pd$multi_line
     , pd$lag_newlines > 0
     )
}

rep_char <- function( char, times ) {
  paste( rep.int( char, times )
       , collapse = ""
       )
}

set_spaces <- function( spaces_after_prefix, force_one ) {
  if ( force_one ) {
    n_of_spaces <- rep( 1, length( spaces_after_prefix ) )
  } else {
    n_of_spaces <- pmax( spaces_after_prefix, 1L )
  }
  n_of_spaces
}

style_space_around_token <- function( pd_flat
                                    , strict
                                    , tokens
                                    , level_before
                                    , level_after = level_before
                                    ) {
  op_after <- pd_flat$token %in% tokens
  op_before <- lead( op_after, default = FALSE )
  idx_before <- op_before & ( pd_flat$newlines == 0L )
  idx_after <- op_after & ( pd_flat$newlines == 0L )
  if ( strict ) {
    pd_flat$spaces[ idx_before ] <- level_before
    pd_flat$spaces[ idx_after ] <- level_after
  } else {
    pd_flat$spaces[ idx_before ] <- pmax( pd_flat$spaces[ idx_before ]
                                        , level_before
                                        )
    pd_flat$spaces[ idx_after ] <- pmax( pd_flat$spaces[ idx_after ]
                                       , level_after
                                       )
  }
  pd_flat
}

wrap_subexpr_in_curly <- function( pd
                                 , ind_to_be_wrapped
                                 , indent_by
                                 , space_after
                                 ) {
  to_be_wrapped_starts_with_comment <- pd$token[ ind_to_be_wrapped[ 1 ] ] == "COMMENT"
  new_expr <- wrap_expr_in_curly( pd[ ind_to_be_wrapped, ]
                                , stretch_out = c( !to_be_wrapped_starts_with_comment
                                                 , TRUE
                                                 )
                                , space_after = space_after
                                )
  new_expr$indent <- max(   pd$indent[ last( ind_to_be_wrapped ) ]
                          - indent_by
                        , 0
                        )
  new_expr_in_expr <- (   new_expr
                      %>% wrap_expr_in_expr()
                      %>% remove_attributes( c( "token_before"
                                              , "token_after"
                                              )
                                           )
                      )
  (   pd
  %>% slice( -ind_to_be_wrapped )
  %>% bind_rows(new_expr_in_expr)
  %>% set_multi_line()
  %>% arrange_pos_id()
  )
}

alignment_serialize <- function( pd_sub ) {
  out <- Map( function( terminal, text, child, spaces, newlines ) {
                if ( terminal ) {
                  paste0( text
                        , rep_char( " ", spaces )
                        )
                } else {
                  paste0( alignment_serialize( child )
                        , rep_char( " ", spaces )
                        )
                }
              }
            , pd_sub$terminal
            , pd_sub$text
            , pd_sub$child
            , pd_sub$spaces
            , pd_sub$newlines
            )
  if ( anyNA( out ) ) {
    return( NA )
  } else {
    paste0( out, collapse = "" )
  }
}

remove_attributes <- function( pd_flat, attributes ) {
  pd_flat[ attributes ] <- rep( list( NULL )
                              , length( attributes )
                              )
  pd_flat
}

set_multi_line <- function( pd ) {
  pd$multi_line <- map_lgl( pd$child, pd_is_multi_line )
  pd
}

wrap_expr_in_curly <- function( pd
                              , stretch_out = c( FALSE, FALSE )
                              , space_after = 1
                              ) {
  if ( is_curly_expr( pd ) ) {
    return( pd )
  }
  if ( stretch_out[ 1 ] ) {
    pd$lag_newlines[ 1 ] <- 1L
  }
  opening <- create_tokens( "'{'"
                          , "{"
                          , pos_ids = create_pos_ids( pd
                                                    , 1
                                                    , after = FALSE
                                                    )
                          , spaces = 1L - as.integer( stretch_out[ 1 ] )
                          , stylerignore = pd$stylerignore[ 1 ]
                          )
  closing <- create_tokens( "'}'"
                          , "}"
                          , spaces = space_after
                          , lag_newlines = as.integer( stretch_out[ 2 ] )
                          , pos_ids = create_pos_ids( pd
                                                    , nrow( pd )
                                                    , after = TRUE
                                                    )
                          , stylerignore = pd$stylerignore[ 1 ]
                          )
  set_multi_line( bind_rows( opening, pd, closing ) )
}

wrap_expr_in_expr <- function( pd ) {
  create_tokens( "expr"
               , ""
               , pos_ids = create_pos_ids( pd
                                         , 1
                                         , after = FALSE
                                         )
               , child = pd
               , terminal = FALSE
               )
}

#' Define reindention input for \code{commastyler_style}
#'
#' @return List of three elements:
#'   \describe{
#'     \item{\code{regex_pattern}}{NULL}
#'     \item{\code{indention}}{default 0}
#'     \item{\code{comments_only}}{TRUE}
#'   }
#' @export
#' @importFrom styler specify_reindention
commastyler_reindention <- function() {
  specify_reindention( regex_pattern = NULL
                     , indention = 0
                     , comments_only = TRUE
                     )
}

lag <- function( x, n = 1L, default = NA ) {
  xlen <- length( x )
  n <- pmin( n, xlen )
  c( rep( default, n )
   , x[ seq_len( xlen - n ) ]
   )
}

lead <- function( x, n = 1L, default = NA ) {
  xlen <- length( x )
  n <- pmin( n, xlen )
  c( x[ -seq_len( n ) ], rep( default, n ) )
}

set_line_break_around_comma <- function( pd ) {
  comma_without_line_break_before <-
    ( ( "','"     == pd$token )
    & ( 0L        == pd$lag_newlines )
    & ( "COMMENT" != pd$token_after )
    & ( "'['"     != lag( pd$token ) )
    )
  pd$lag_newlines[ comma_without_line_break_before ] <- 1L
  pd$lag_newlines[ lag( comma_without_line_break_before ) ] <- 0L

  if ( any( comma_without_line_break_before ) ) {
    rparen_without_line_break_before <-( "')'" == pd$token
                                       & 0L    == pd$lag_newlines
                                       )
    pd$lag_newlines[ rparen_without_line_break_before ] <- 1L
  }
  pd
}

set_line_break_before_curly_opening_style <- function() {
  create_style_guide(line_break = list(set_line_break_before_curly_opening
                                      , set_line_break_around_comma
                                      #, set_indention_
                                      )
                    , style_guide_name = "commastyler::set_line_break_before_curly_opening_style@https://github.com/jdnewmil/commastyler"
                    , style_guide_version = packageVersion("commastyler")
                    )
}

remove_line_break_before_round_closing_after_curly <- function( pd ) {
  round_after_curly <- ( pd$token == "')'"
                       & ( pd$token_before == "'}'" )
                       )
  pd$lag_newlines[round_after_curly] <- 0L
  pd
}

remove_line_break_before_round_closing_fun_dec <- function( pd ) {
  if ( is_function_dec( pd ) ) {
    round_after <- ( pd$token == "')'"
                   & pd$token_before != "COMMENT"
                   )
    pd$lag_newlines[ round_after ] <- 0L
  }
  pd
}

build_space_manipulators <- function( scope
                                    , strict
                                    , indent_by
                                    , math_token_spacing
                                    , start_comments_with_one_space
                                    ) {
  if ( scope >= "spaces" ) {
    lst( indent_braces = partial( indent_braces
                                , indent_by = indent_by
                                )
       , unindent_fun_dec
       , indent_op = partial( indent_op
                            , indent_by = indent_by
                            )
       , indent_eq_sub = partial( indent_eq_sub
                                , indent_by = indent_by
                                )
       , indent_without_paren = partial( indent_without_paren
                                       , remove_space_before_opening_paren =
                                          if (strict)
                                            remove_space_before_opening_paren
                                       , add_space_after_for_if_while
                                       , add_space_before_brace
                                       , remove_space_before_comma
                                       , style_space_around_math_token =
                                          partial( style_space_around_math_token
                                                 , strict
                                                 , math_token_spacing$zero
                                                 , math_token_spacing$one
                                                 )
                                       , style_space_around_tilde =
                                          partial( style_space_around_tilde
                                                 , strict = strict
                                                 )
                                       , spacing_around_op =
                                          purrr::partial( set_space_around_op
                                                        , strict = strict
                                                        )
                                       , addone_space_after_opening_paren
                                       , remove_space_after_excl
                                       , set_space_after_bang_bang
                                       , remove_space_before_dollar
                                       , remove_space_after_fun_dec
                                       , remove_space_around_colons
                                       , start_comments_with_space =
                                          partial( start_comments_with_space
                                                 , force_one = start_comments_with_one_space
                                                 )
                                       , remove_space_after_unary_pm_nested
                                       , spacing_before_comments =
                                          if (strict) {
                                            set_space_before_comments
                                          } else {
                                            add_space_before_comments
                                          }
                                       , set_space_between_levels
                                       , set_space_between_eq_sub_and_comma
                                       , set_space_in_curly_curly
                                       )
     )
  }
}

#' Build a list of functions to address line breaks
#'
#' @param scope Ordered factor scalar, see \code{\link{commastyler_style}}
#' @param strict Logical scalar, see \code{\link{commastyler_style}}
#' @return List of functions or NULL
#' importFrom tibble lst
build_line_break_manipulators <- function( scope, strict ) {
  if ( scope >= "line_breaks" ) {
    lst( set_line_break_around_comma
       , set_line_break_before_curly_opening
       , remove_line_break_before_round_closing_after_curly = if (strict)
         remove_line_break_before_round_closing_after_curly
       , remove_line_break_before_round_closing_fun_dec = if (strict)
         remove_line_break_before_round_closing_fun_dec
       , style_line_break_around_curly = partial( style_line_break_around_curly
                                                , strict
                                                )
       , set_line_break_around_curly_curly
       , set_line_break_after_opening_if_call_is_multi_line = if (strict) {
          partial( set_line_break_after_opening_if_call_is_multi_line
                 , except_token_after = "COMMENT"
                 , except_text_before = c( "switch"
                                         , "ifelse"
                                         , "if_else"
                                         )
                 )
         }
       , set_line_break_before_closing_call = if (strict) {
          partial( set_line_break_before_closing_call
                 , except_token_before = "COMMENT"
                 )
         }
       , remove_line_break_in_empty_fun_call
       , add_line_break_after_pipe = if (strict)
          add_line_break_after_pipe
       , set_linebreak_after_ggplot2_plus = if (strict)
          set_linebreak_after_ggplot2_plus
       )
  }
}


#' Define "commastyler" style
#'
#' Modeled after \code{\link[styler]{tidyverse_style}}.
#'
#' @param scope Character scalar, the extent of manipulation. Can range from
#'   "none" (least invasive) to "token" (most invasive). This
#'   argument is a vector of length one.
#'   The following options for \code{scope} are available.
#'   \describe{
#'     \item{\code{"none"}}{Performs no transformation at all.}
#'     \item{\code{"spaces"}}{Manipulates spacing between token on the same
#'       line.}
#'     \item{\code{"indention"}}{In addition to "spaces", this option also
#'       manipulates the indention level.}
#'     \item{\code{"line_breaks"}}{In addition to "indention", this option
#'       also manipulates line breaks.}
#'     \item{\code{"tokens"}}{In addition to "line_breaks", this option also
#'       manipulates tokens.}
#'   }
#'   Note that more invasive operations can only be performed if all less
#'   invasive operations are also performed.
#'
#' @param strict A logical value indicating whether a set of strict or not so
#'   strict transformer functions should be returned. Compare the functions
#'   returned with or without strict = TRUE. For example, strict = TRUE means
#'   force one space e.g. after "," and one line break e.g. after a closing
#'   curly brace. strict = FALSE means to set spaces and line breaks to one
#'   if there is none and leave the code untouched otherwise. See 'Examples'.
#' @param indent_by How many spaces of indention should be inserted after
#'   operators such as '('.
#' @param start_comments_with_one_space	Whether or not comments should start
#'   with only one space (see start_comments_with_space()).
#' @param reindention	 A list of parameters for regex re-indention, most
#'   conveniently constructed using specify_reindention().
#' @param math_token_spacing A list of parameters that define spacing around
#'   math token, conveniently constructed using specify_math_token_spacing().
#' @return A style, usable by \code{\link[styler]{style_text}}.
#' @export
#' @importFrom tibble lst new_tibble
#' @importFrom stats lag
#' @importFrom utils packageVersion
#' @importFrom styler create_style_guide default_style_guide_attributes
#'   tidyverse_math_token_spacing tidyverse_reindention
#' @importFrom dplyr %>% bind_rows if_else last slice
#' @importFrom rlang is_empty seq2 with_handlers
#' @importFrom purrr map partial when compact map_chr map_int map_lgl
#' @importFrom rematch2 re_match
commastyler_style <- function( scope = "tokens"
                             , strict = TRUE
                             , indent_by = 2
                             , start_comments_with_one_space = FALSE
                             , reindention = commastyler_reindention()
                             , math_token_spacing = tidyverse_math_token_spacing()
                             ) {
  scope <- character_to_ordered( scope
                               , c( "none", "spaces", "indention"
                                  , "line_breaks", "tokens"
                                  )
                               )
  space_manipulators <- build_space_manipulators( scope
                                                , strict
                                                , indent_by
                                                , build_space_manipulators
                                                , start_comments_with_one_space
                                                )
  use_raw_indention <- scope < "indention"
  line_break_manipulators <- build_line_break_manipulators( scope, strict )
  token_manipulators <- if (scope >= "tokens") {
    lst( fix_quotes
       , force_assignment_op
       , resolve_semicolon
       , add_brackets_in_pipe
       , remove_terminal_token_before_and_after
       , wrap_if_else_while_for_fun_multi_line_in_curly = if (strict)
          wrap_if_else_while_for_fun_multi_line_in_curly
       )
  }
  indention_modifier <-
    lst( update_indention_ref_fun_dec = if ( scope >= "indention" )
          update_indention_ref_fun_dec
       )
  style_guide_name <- "commastyler::commastyler_style@https://github.com/jdnewmil/commastyler"
  create_style_guide( initialize = default_style_guide_attributes
                    , line_break = line_break_manipulators
                    , space = space_manipulators
                    , token = token_manipulators
                    , indention = indention_modifier
                    , use_raw_indention = use_raw_indention
                    , reindention = reindention
                    , style_guide_name = style_guide_name
                    , style_guide_version = packageVersion( "commastyler" )
                    )
}
