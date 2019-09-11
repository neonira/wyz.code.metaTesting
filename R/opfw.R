opwf <- function(fun_f_1,
                 parameterNames_s,
                 functionName_s_1 = NA_character_) {

  qfa <- qualifyFunctionArguments(fun_f_1)
  l <- length(parameterNames_s)
  lfa <- length(qfa$arguments)
  if (lfa != l)
    abort('function owns', lfa, 'arguments, you provided', l, 'arguments')

  if (l == 0) return(fun_f_1) # no work on functions without arguments

  if (l > 0) {
    rv <- sapply(seq_len(l), function(k) {
      x <- wyz.code.offensiveProgramming::FunctionParameterName(parameterNames_s[k])
      x$isSemanticName()
    })
    if (!all(rv)) abort('provided parameter names are not all semantic names',
                        strBracket(paste(parameterNames_s[!rv], collapse = ', ')))
  }

  ff <- qfa$arguments
  names(ff) <- parameterNames_s

  tt <- buildArgumentsSubstitutionTable(fun_f_1)
  wp <- which(tt$requires_name_propagation == TRUE)
  lwp <- length(wp)
  audit <- wyz.code.offensiveProgramming::isAuditable()
  if (lwp > 0) {

    subargs <- removeEllipsisName(parameterNames_s)
    sfa <- removeEllipsisName(qfa$argument_names)

    z <- sapply(seq_len(lwp), function(k) {
      lng <- tt$text[wp[k]]
      if (audit) cat('>>> patching', strBracket(lng), 'with', strBracket(strJoin(sfa)),
                     'to be replaced by', strBracket(strJoin(subargs)), '\n')
      lng <- codeScanner(lng, sfa, subargs)
      if (audit) cat('>>> result', lng, '\n')
      lng
    })
    #print(z)
    ff[wp] <- str2expression(z)
  }

  callParameters <- function() {
    sapply(seq_len(l), function(k) {
      if (is.symbol(qfa$arguments[[k]])) parameterNames_s[k] else {
        paste(qfa$argument_names[k], '=', parameterNames_s[k])
      }
    })
  }

  f <- function() {}
  formals(f) <- ff
  dsf <- ifelse(is.na(functionName_s_1), deparse(substitute(fun_f_1)), functionName_s_1)
  e <- str2lang(paste0(dsf, '(', paste(callParameters(), collapse = ', '),')'))
  body(f)  <- as.call(c(as.name('{'), e))

  # some errors might remain in code translation - uneasy to detect them as it
  # implies function execution with valued arguments.
  f
}

offensiveProgrammingWrapFunction <- opwf



