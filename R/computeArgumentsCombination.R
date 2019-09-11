computeArgumentsCombination <- function(fun_f_1) {

  combine <- function(v_s) {
    l <- append(
       unlist(lapply(seq_len(length(v_s)), function(k) {
          combn(v_s, k, simplify = FALSE)
       }), FALSE),
       list(character(0)), 0)
    lapply(l, function(e) if (length(e) <= 1) e else paste(e, collapse = ', '))
  }

  spa <- function(a_s_1, b_s, ...) {
    if (length(a_s_1) == 0) return(b_s)
    if (length(b_s) == 0) return(a_s_1)
    paste(a_s_1, paste(b_s, collapse = ', '), ..., sep = ', ', collapse = ', ')
  }

  qfa <- qualifyFunctionArguments(fun_f_1)

  ar <- length(qfa$stripped_symbol_names) # mandatory arguments
  as <- qfa$stripped_symbol_names

  er <- if (qfa$owns_ellipsis == TRUE) 0:3 else 0
  mer <- max(er)
  en <- buildEllipsisNames(3)
  es <- if (mer == 0) list(character(0)) else list(
    character(0),
    en[1],
    spa(en[1], en[2]),
    spa(en[1], en[2], en[3])
  )

  l <- length(qfa$default_names)
  dr <- if (l == 0) 0 else seq.int(0, l)
  mdr <- max(dr)
  ds <- if (mdr == 0) list(character(0)) else combine(qfa$default_names)


  varsig <- if (mer == 0) ds else {
      if (mdr == 0) es else {
        unlist(lapply(es, function(e) {
          lapply(ds, function(h) {
            spa(e, h)
          })
        }), FALSE)
      }
    }

  # cat('varsing', strBracket(strJoin(varsig)), '\n')

  ax <- paste(as, collapse = ', ')
  sig <- if (ar == 0) varsig else  {
    if (length(varsig) == 0) ax else {
      lapply(varsig, function(e) {
        if (length(e) == 0) ax else unlist(lapply(e, function(z) spa(ax, z)), FALSE)
      })
    }
  }

  list(names = list(
    argument = unlist(as),
    ellipsis = unlist(es),
    default = qfa$default_names
  ),
  number = list(
    argument = ar,
    ellipsis = er,
    default = dr
  ),
  signatures = sig
  )
}