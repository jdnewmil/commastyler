# support.R

can_verify_roundtrip <- function( transformers ) {
  is.null( transformers$token )
}

verify_roundtrip <- function( old_text, new_text ) {
  if ( !expressions_are_identical( old_text, new_text ) ) {
    msg <- paste( "The expression evaluated before the styling is not the"
                , "same as the expression after styling. This should not"
                , "happen. Please file a bug report on GitHub"
                , "(https://github.com/r-lib/styler/issues)"
                , "using a reprex."
                )
    stop( msg )
  }
}

expressions_are_identical <- function( old_text, new_text ) {
  identical( parse_safely( old_text
                         , keep.source = FALSE
                         )
           , parse_safely( new_text
                         , keep.source = FALSE
                         )
           )
}

parse_safely <- function( text, ... ) {
  tried_parsing <- with_handlers( parse( text = text, ...)
                                , error = function(e) e
                                , warning = function(w) w
                                )
  if ( inherits( tried_parsing, "error" ) ) {
    if ( has_crlf_as_first_line_sep( tried_parsing$message
                                   , text
                                   )
       ) {
      stop( paste0( "The code to style seems to use Windows style line"
                  , "endings (CRLF). styler currently only supports"
                  , "Unix style line endings (LF). Please change the"
                  , "EOL character in your editor to Unix style and"
                  , "try again.\nThe parsing error was:\n"
                  , tried_parsing$message
                  )
         )
    } else {
      stop( tried_parsing$message )
    }
  } else if ( inherits( tried_parsing, "warning" ) ) {
    warn( tried_parsing$message )
  }
  tried_parsing
}

