plt <- vector("list", 5)
i = 1

grab_grob <- function(){
    grid.echo()
    grid.grab()
}

Error in gList(list(list(data = list(timestamp = c(1546294086, 1546294985,  : 
                                                       only 'grobs' allowed in "gList"
for (device in devices){
    for (unit in units){
        for (type in types){
            for (solName in solNames){
                # pltMonth(paste(solName, type, device, unit, sep=""))
                plots = pltMonth(paste(solName, type, device, unit, sep=""))
                plt[[i]] <- plots
                i <- i + 1
            }
            pdf(paste(type, device, unit, ".pdf", sep=""),
                width=8,
                height=15)
            grid.arrange(
                arrangeGrob(plt[1],plt[2],plt[3],plt[4],plt[5],nrow=5,heights=c(.2,.2,.2,.2,.2))
            )
            dev.off()
            plt[] <- NULL
            i = 0
            break
        }
        break
    }
    break
}