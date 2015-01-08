total <- 10

pb <- winProgressBar(title = "progress bar", min = 0,
                     max = total, width = 300)

for(i in 1:total){
  Sys.sleep(0.5)
  setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),
                                        "% done"))
}
close(pb)
