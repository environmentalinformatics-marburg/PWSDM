########### FUNCTIONS ####################

makpat <- function(hc, num){ #hc can be "h" for HDM or "c" for CDM, num=length(list of shapes)
  patcb <- lapply(c(2:num), function(i){
    noquote(paste0(hc, "dmcntrm[[",i,"]]@data[ncol(", hc, "dmcntrm[[",i,"]]@data)],"))
  })
  patcb[[num-1]] <- substr(patcb[[num-1]], 1, nchar(patcb[[num-1]])-1)
  pc <- unlist(patcb)
  pc <- noquote(pc)
}


mymerge <- function(shape, zonal,i, grid){ #function for merging
  m1 <- merge(shape, zonal[[i]],  by.x="ID", by.y="zone")
  names(m1)[length(names(m1))] <- names(grid[[i]])
  print(i)
  return(m1)
}
