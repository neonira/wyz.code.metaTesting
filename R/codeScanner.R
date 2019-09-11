codeScanner <- function(code_s_1, originArgumentName_s, targetArgumentName_s, asList_b_1 = FALSE) {
  stopifnot(all(length(originArgumentName_s) == length(targetArgumentName_s)))

  chooseMarker <- function(code_s_1) {
    markers <- c('\u03DF', '\u03D9', '\u03E1') # koppa, qoppa, sampi
    v <- sapply(markers, function(e) grepl(e, code_s_1, fixed = TRUE))
    w <- which(v == FALSE)
    if (length(w) == 0) abort('none of the three marks is free')
    markers[w[1]]
  }

  identifyConstantParts <- function() {
    regmatches(code_s_1, gregexpr('[\'"](.*?)[\'"]|#(.*)', code_s_1[1], perl = TRUE))[[1]]
  }

  substituteConstantParts <- function(code_s_1, marker_s_1) {

    r <- identifyConstantParts()
    lr <- length(r)
    code <- code_s_1
    if (lr != 0) {
      sq <- seq_len(length(r))
      marker_names <- paste0(marker_s_1, sq, marker_s_1)

      sapply(sq, function(k) {
        code <<- sub(r[k], marker_names[k], code, fixed = TRUE)})
    }
    list(code = code, parts = r,
         marker = marker_s_1,
         marker_names = if (lr > 0) marker_names else NA_character_,
         back_substitution = lr > 0)
  }

  identifyArguments <- function(code_s_1) {
    # arguments in function calls are followed by , or ) - mandatory
    oanp <- paste(originArgumentName_s, sep = '', collapse = '|')
    gp <- paste0('[\\(\\,+/*-^:%<>=]*\\s*(', oanp, ')\\s*')
    regmatches(code_s_1, gregexpr(gp, code_s_1[1], perl = TRUE))[[1]]
  }

  substituteArguments <- function(code_s_1, marker2_s_1) {
    r <- identifyArguments(code_s_1)
    lr <- length(r)
    code <- code_s_1
    if (lr != 0) {
      sq <- seq_len(length(r))
      marker_names <- paste0(marker2_s_1, sq, marker2_s_1)

      sapply(sq, function(k) {
        code <<- sub(r[k], marker_names[k], code, fixed = TRUE)})
    }
    list(code = code, parts = r,
         marker = marker2_s_1,
         marker_names = if (lr > 0) marker_names else NA_character_,
         back_substitution = lr > 0)
  }

  substituteBack <- function(code_s_1, pass1_l, pass2_l) {

    subOnPass <- function(code_s_1, pass_l, workOnArguments_s_1 = FALSE) {
      code <- code_s_1
      if (pass_l$back_substitution) {
        co <- if (workOnArguments_s_1) {
          tmp <- pass_l$parts
          sapply(seq_len(length(originArgumentName_s)), function(j) {
            # tmp <<- gsub(paste0('([^.@]|::+)', originArgumentName_s[j]),
            #              paste0('\\1', targetArgumentName_s[j]), tmp, perl = TRUE)
            tmp <<- gsub(paste0('([^:][:^+%*/-])?', originArgumentName_s[j]),
                          paste0('\\1', targetArgumentName_s[j]), tmp, perl = TRUE)
          })
          tmp
        } else pass_l$parts

        sp <- if (workOnArguments_s_1) targetArgumentName_s else pass_l$parts
        sq <- seq_len(length(pass_l$parts))
        sapply(sq, function(k) {
          code <<- sub(pass_l$marker_names[k], co[k], code, fixed = TRUE)
        })
      }
      code
    }

    # keep inverted order
    code <- subOnPass(code_s_1, pass2_l, TRUE)
    subOnPass(code, pass1_l)
  }

    # no match
    b <- sapply(originArgumentName_s, function(e) {
      grepl(e, code_s_1, fixed = TRUE)
    })
    if (all(!b)) return(code_s_1)

    # simple argument matching
    w <- sapply(seq_len(length(originArgumentName_s)), function(k) {
      if (grepl(paste0('^\\s*', originArgumentName_s[k], '\\s*$'), code_s_1, perl = TRUE))
        return(targetArgumentName_s[k])
      NA
    })
    ww <- Filter(Negate(is.na), w)
    if (length(ww) > 0) return(ww[1])

    marker <- chooseMarker(code_s_1)
    pass1 <- substituteConstantParts(code_s_1, marker)
    marker2 <- chooseMarker(pass1$code)
    pass2 <- substituteArguments(pass1$code, marker2)
    pass3 <- substituteBack(pass2$code, pass1, pass2)
    if (asList_b_1) {
      list(constant_parts = pass1,
         argument_parts = pass2,
         result = pass3)
    } else pass3

}
