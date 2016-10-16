cols1 = c("#edd9c0",
          "#c9d8c5",
          "#a8b6bf",
          "#7d4627",
          "#506C61",
          "#E06C50",
          "#004073")

par.list = list(bg = "white", #col = cols1[7], col.axis = cols1[7],
                #col.lab = cols1[7], col.main  = cols1[7], col.sub = cols1[7],
                las = 1,
                lwd = 2,
                cex = 1.1,
                cex.axis = 1.1,
                cex.lab = 1.1,
                yaxs="i",mgp = c(2.5,.5,0), tcl = -0.25,
                mar=c(4.5,4.5,4.5,1))

par.list2 = par.list
par.list2[['mar']] = c(4.5,1,1,1)
