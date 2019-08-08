

# xlevels = factorlevels
# format.factor <- "level"
# format.scale <- "var"
# format.scale <- "var"
# format.contrast <- "level vs ref"
# sep = "::"

parse_interaction_terms <- function(
  terms,
  xlevels,
  format.factor,
  format.contrast,
  format.scale,
  sep = "::",
  ...
){
  
  if (any(attr(terms, "order") > 2))
    stop("Interaction terms with order greater than 2 are not supported.")
  
  ilabs <- attr(terms, "term.labels")[attr(terms, "order") == 2]
  inter.list <- strsplit(ilabs, ":")
  intervars <- unique(unlist(inter.list))
  
  
  if (length(inter.list) > 0) {
    
    if (missing(format.factor)) format.factor <- "level"
    if (missing(format.scale)) format.scale <- "var"
    if (missing(format.contrast)) format.contrast <- "level vs ref"
    
    format.factor <- sub("var", "%s", format.factor)
    format.factor <- sub("level", "%s", format.factor)
    
    format.contrast <- sub("level", "%s", format.contrast)
    format.contrast <- sub("ref", "%s", format.contrast)
    format.contrast <- sub("var", "%s", format.contrast)
    
    format.scale <- sub("var", "%s", format.scale)
    
    iterms <- lapply(inter.list, function(vv) {
      
      v1 <- vv[1]; ref1 <- xlevels[[v1]][[1]]
      v2 <- vv[2]; ref2 <- xlevels[[v2]][[1]]
      
      if (is.null(ref1)) {
        # first variable is continuous
        if (is.null(ref2)) {
          # second variable is continuous
          stop(
            paste(
              "Can only handle interactions when at least one variable is a factor.\nBut argument xlevels contains no entry for either",
              v1,
              "or",
              v2
            )
          )
        } else {
          
          levs2 <- xlevels[[v2]]
          labs <- sapply(levs2, function(l) {
            paste(
              sprintf(format.scale,  v1),
              sprintf(format.factor, v2, l),
              sep = sep
            )
          })
          
          contrast <- lapply(1:length(levs2), function(l) {
            if (l == 1)
              
              paste(v1)
            
            else
              
              c(
                v1,
                paste0(
                  v1,
                  ":",
                  paste(v2, levs2[[l]], sep = ""),
                  sep = ""
                )
              )
            
          })
          
          names(contrast) <- labs
          attr(contrast, "variables") <- c(v1, v2)
          return(contrast)
          
        }
      } else {
        if (is.null(ref2)) {
          
          levs1 <- xlevels[[v1]]
          
          labs <- paste(
            sprintf(format.factor, v1),
            levs1,
            sprintf(format.scale, v2),
            sep = sep
          )
          
          contrast <- lapply(1:length(levs1), function(l) {
            if (l == 1){
              v2
            } else {
              c(
                v2,
                paste0(
                  paste0(v1, levs1[[l]]),
                  ":",
                  v2
                )
              )
            }
          })
          
          names(contrast) <- labs
          
          attr(contrast, "variables") <- c(v1,v2)
          
          
          return(contrast)
          
        } else {
          
          levs1 <- xlevels[[v1]]
          levs2 <- xlevels[[v2]]
          
          labs1 <- paste(
            rep(
              sprintf(format.factor, levs1),
              rep(length(levs2) - 1, length(levs1))
            ),
            sprintf(
              format.contrast,
              levs2[-1],
              levs2[1]
            ),
            sep = sep
          )
          
          contrast1 <- map(
            1:length(levs1),
            .f = function(l1){
              if(l1==1){
                map(
                  2:length(levs2),
                  .f = function(l2){
                    paste(v2, levs2[l2], sep = "")
                  }
                )
              } else {
                map(
                  2:length(levs2),
                  .f = function(l2){
                    c(
                      paste(v2, levs2[l2], sep = ""),
                      paste(
                        paste(v1, levs1[l1], sep = ""),
                        ":", paste(v2, levs2[l2], sep = ""),
                        sep = ""
                      )
                    )
                  }
                )
              }
            }
          ) %>%
            flatten() %>%
            set_names(labs1)
          
          labs2 <- paste(
            rep(
              sprintf(format.factor, levs2),
              rep(length(levs1) - 1, length(levs2))
            ),
            sprintf(format.contrast, levs1[-1], levs1[1]),
            sep = sep
          )
          
          contrast2 <- map(
            seq_along(levs2),
            .f = function(l2){
              if(l2 == 1){
                map(
                  2:length(levs1),
                  .f = function(l1){
                    paste(v1, levs1[l1], sep = "")
                  }
                )
              } else {
                map(
                  2:length(levs1),
                  .f = function(l1){
                    c(
                      paste(v1, levs1[l1], sep = ""),
                      paste(
                        paste(v1, levs1[l1], sep = ""),
                        ":",
                        paste(v2, levs2[l2], sep = ""),
                        sep = ""
                      )
                    )
                  }
                )
              }
            }
          ) %>%
            flatten() %>%
            set_names(labs2)
          
          names(contrast1) %<>% paste(v1, ., sep = "::")
          names(contrast2) %<>% paste(v2, ., sep = "::")
          
          contrast <- c(contrast1, contrast2)
          attr(contrast, "variables") <- c(v1, v2)
          return(contrast)
        }
      }
    })
    
    names(iterms) <- ilabs
    iterms
    
  }
  
}
