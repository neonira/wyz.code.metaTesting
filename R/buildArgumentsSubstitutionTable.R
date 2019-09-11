buildArgumentsSubstitutionTable <- function(fun_f_1) {
  mfa <- wyz.code.offensiveProgramming::retrieveFunctionArguments(fun_f_1)
  tk <- data.table(
    argname = names(mfa),
    class = unlist(lapply(mfa, class)),
    typeof = unlist(lapply(mfa, typeof)),
    mode = unlist(lapply(mfa, mode)),
    iscall = unlist(lapply(mfa, is.call)),
    issymbol = unlist(lapply(mfa, is.symbol)),
    isexpression = unlist(lapply(mfa, is.expression)),
    text = as.character(mfa)
  )

  escapeDot <- function(x_s) gsub('.', '\\.', x_s, fixed = TRUE)

  argname <- text <- NULL # data.table NSE issue with Rcmd check
  tk[, `:=`(requires_name_propagation = grepl(paste(escapeDot(argname), collapse = '|'), text, perl = TRUE))]
  tk
}
