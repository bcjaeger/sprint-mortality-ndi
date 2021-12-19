



kmplot <- function(km, cumHaz=FALSE, mark=3, simple=FALSE,
                   xaxis.at=pretty(km$time), xaxis.lab=xaxis.at,
                   lty.surv=1, lwd.surv=1, col.surv=1,
                   lty.ci=0, lwd.ci=1, col.ci=col.surv, #By default (lty.ci=0), confidence intervals are not plotted.
                   group.names=NULL, group.order=seq(length(km$n)), extra.left.margin=4,
                   label.n.at.risk=FALSE, draw.lines=TRUE, cex.axis=1,
                   xlab='', ylab='', main='', xlim=c(0,max(km$time)), ylim=c(0,1),
                   grid=TRUE, grid.vert=TRUE, lty.grid=1, lwd.grid=1, col.grid=grey(.9),
                   legend=!is.null(km$strata), loc.legend='topright', add=FALSE,
                   yaxis.at, yaxis.lab, leg.title, cex.text, add.zeroes=FALSE, no.ci=FALSE, cex.legend=1.0,
                   use.grp.4legend=TRUE, legend.names, extra.top=0,
                   move.lab.left=0, trunc.time.at, leg.ncol=1, extra.bottom = 0, nrisk.down = 0,
                   ... # ... is passed to par()
){
  # Version 2.5.5: 2014/5/19

  # km is the output from survfit() function in survival package.

  # xaxis.at specifies where 'n at risk' will be computed and printed.
  # xaxis.lab specifies what will be printed at xaixs.at.  (see example)

  # If group names are long, add extra left margin by setting extra.left.margin to something greater than 0.

  # line specifications (lty.surv, lwd.surv, col.surv) will be recycled.
  # Set lty.ci to 1 if confidence intervals are needed.
  # group.names will overwrite whatever is specified in survfit() output.
  # group.order specifies the order of groups from top in 'n at risk'.  1 is at top, 2 next, and so on.

  # if add=TRUE, then par() is not refreshed.  allows multiple panels by
  # using, e.g., par(mfrow=c(2,2)).

  # op <- par(no.readonly = TRUE)

  ng0 <- length( km$strata ) ; ng <- max(ng0,1)
  # When only one group...
  if(ng0==0){ km$strata <- length(km$time) ; names(km$strata) <- 'All' ; legend <- draw.lines <- FALSE }

  lty.surv <- rep(lty.surv, ng) ; lwd.surv <- rep(lwd.surv, ng) ; col.surv <- rep(col.surv, ng)
  lty.ci <- rep(lty.ci, ng) ;     lwd.ci <- rep(lwd.ci, ng)     ; col.ci <- rep(col.ci, ng)

  ## group names and error checking
  gr <- c(km$strata)
  if( is.null(group.names) ){ group.names <- names(km$strata) }
  if( length(unique(group.names)) != ng ){ stop('\n','length(unique(group.names)) != number of groups.') }
  if( suppressWarnings(any( sort(group.order) != 1:ng)) )
  { stop('\n', 'Something wrong with group.order.','\n','sort(group.order) must equal 1:', ng, '.') }
  group.names <- gsub(' *$', '', group.names)  #to remove unwanted white spaces in group.names.
  if(ng==1 & (group.names[1]=='group.names') ){ group.names <- 'N at risk' ; label.n.at.risk = FALSE }

  ## graphic parameters
  if(!add){
    par(list(oma=c(1,1,1,1), mar=c(4+ng+extra.bottom,4+extra.left.margin,1+extra.top,1)+.1))
    if(simple) par( mar=c(3,4,2,1)+.1 )
    par( list(...) )
  }else{
    par(mar=c(4+ng+extra.bottom,4+extra.left.margin,1+extra.top,1)+.1)
  }

  ## reformat survival estimates
  dat <- data.frame(time=km$time, n.risk=km$n.risk, n.event=km$n.event, survival=km$surv, std.err=km$std.err,
                    lower=km$lower, upper=km$upper, group=rep( group.names, gr) )
  dat$cumHaz<-with(dat,(-1)*log(survival))
  dat$cumHazL<-with(dat, (-1)*log(lower))
  dat$cumHazU<-with(dat, (-1)*log(upper))
  if(is.na(trunc.time.at)==FALSE){
    dat<-subset(dat, time<=trunc.time.at)
  }
  dat.list <- split(dat, f=dat$group)

  ## plot (but not survival curves)
  plot(0,type='n', xlim=xlim, ylim=ylim, xaxt='n', yaxt='n', xlab='', ylab='', main=main)
  if(grid){
    par('xpd'=FALSE)
    if(grid.vert){
      abline(v=xaxis.at, lty=lty.grid, lwd=lwd.grid, col=col.grid)
    }
    abline(h=yaxis.at, lty=lty.grid, lwd=lwd.grid, col=col.grid )
  }
  axis( side=2, at=yaxis.at, label=yaxis.lab, cex.axis=cex.axis, las=1)
  axis( side=1, at=xaxis.at, label=xaxis.lab, line=-0.5, tick=FALSE, cex.axis=cex.axis )
  axis( side=1, at=xaxis.at, label=rep('',length(xaxis.at)), line=0, tick=TRUE )
  title(xlab=xlab, line=1.5, adj=.5, ...) ; title(ylab=ylab, ... )

  if(!simple){
    ## write group names
    group.name.pos <- (par()$usr[2]-par()$usr[1]) / -8 ; padding <- abs( group.name.pos / 8 )
    line.pos <- (1:ng)[order(group.order)] + 2 + nrisk.down
    if(use.grp.4legend){
      mtext( group.names, side=1, line=line.pos, at=group.name.pos+move.lab.left, adj=1, col=1, las=1, cex=cex.text)
    }else{
      mtext( legend.names, side=1, line=line.pos, at=group.name.pos+move.lab.left, adj=1, col=1, las=1, cex=cex.text)
    }

    ## draw matching lines for n at risk. [original is 0-2*padding, may need to change back]
    if(draw.lines){
      par('xpd'=TRUE)
      for(i in 1:ng){
        axis(side=1, at=c(group.name.pos+padding,0-2*padding)+move.lab.left, labels=FALSE, line=line.pos[i]+0.6, lwd.ticks=0,
             col=col.surv[i], lty=lty.surv[i], lwd=lwd.surv[i] ) }
    }

    ## numbers at risk
    kms <- summary(km, times=xaxis.at) ; if(is.null(kms$strata)) kms$strata <- rep(1,length(kms$time) )
    d1 <- data.frame(time = kms$time, n.risk = kms$n.risk, strata = c(kms$strata))
    d2 <- split(d1, f=d1$strata)

    ## Right-justifying the numbers
    ndigits <- lapply(d2, function(x) nchar(x[,2]) )
    max.len <- max( sapply(ndigits, length) )
    L <- do.call('rbind', lapply(ndigits, function(z){ length(z) <- max.len ; z} ) )
    nd <- apply( L, 2, max, na.rm=T )
    for( i in seq(ng) ){
      this <- d2[[i]]
      w.adj <- strwidth('0', cex=cex.axis, font=par('font')) / 2 * nd[1:nrow(this)]
      #print(w.adj)
      mtext( side=1, at=this$time+w.adj, text=this$n.risk, line=line.pos[i], cex=cex.text, adj=1, col=1, las=1)
      if(add.zeroes){
        cat(w.adj[7]," : ",line.pos[i],"\n")
        mtext(side=1, at=5+(w.adj[5]/2), text="0", line=line.pos[i], cex=cex.text, adj=1, col=1, las=1)
      }
    }
    if(label.n.at.risk) mtext( side=1, text='N at risk', at=group.name.pos, line=1.5, adj=1, col=1, las=1, cex=cex.text)
  } ## End of if(!simple)

  # Legend
  rlp <- group.order
  if(legend){
    bgc <- ifelse( par('bg')=='transparent', 'white', par('bg') )
    if(leg.title!=" "){
      if(use.grp.4legend){
        legend(x=loc.legend, legend=group.names[rlp], col=col.surv[rlp], lty=lty.surv[rlp], lwd=lwd.surv[rlp],
               bty='o', bg=bgc, ncol=leg.ncol, box.col='transparent', inset=.01, title=leg.title, cex=cex.legend)
      }else{
        legend(x=loc.legend, legend=legend.names[rlp], col=col.surv[rlp], lty=lty.surv[rlp], lwd=lwd.surv[rlp],
               bty='o', bg=bgc, ncol=leg.ncol, box.col='transparent', inset=.01, title=leg.title, cex=cex.legend)
      }
    }else{
      if(use.grp.4legend){
        legend(x=loc.legend, legend=group.names[rlp], col=col.surv[rlp], lty=lty.surv[rlp], lwd=lwd.surv[rlp],
               bty='o', bg=bgc, ncol=leg.ncol, box.col='transparent', inset=.01, cex=cex.legend)
      }else{
        legend(x=loc.legend, legend=legend.names[rlp], col=col.surv[rlp], lty=lty.surv[rlp], lwd=lwd.surv[rlp],
               bty='o', bg=bgc, ncol=leg.ncol, box.col='transparent', inset=.01, cex=cex.legend)
      }
    }
  }

  ## draw confidence intervals
  #if(no.ci==FALSE){
  #  for(i in 1:ng){
  #    this <- dat.list[[i]]
  #    x <- this$time ; L <- this$lower ; U <- this$upper ; S <- this$survival
  #    naL <- which( is.na(L) ) ; L[naL] <- L[naL-1] ; U[naL] <- U[naL-1]
  #    lines( x, L, type='s', col=col.ci[i], lty=lty.ci[i], lwd=lwd.ci[i] )
  #    lines( x, U, type='s', col=col.ci[i], lty=lty.ci[i], lwd=lwd.ci[i] )
  #  }
  #}

  # draw curves
  par(xpd=FALSE)
  if(cumHaz==FALSE){
    for(i in 1:ng){
      this <- subset(dat, group==group.names[i])
      x <- this$time ; CH <- this$survival; CHL<-this$lower; CHU<-this$upper
      lines(x, CH, type='s', col=col.surv[i], lty=lty.surv[i], lwd=lwd.surv[i])
      if(no.ci==FALSE){
        lines(x, CHL, type='s', col=col.surv[i], lty=2, lwd=0.90)
        lines(x, CHU, type='s', col=col.surv[i], lty=2, lwd=0.90)
        for(j in 1:(length(x)-1)){
          polygon(x=c(x[j],x[j],x[j+1],x[j+1]), y=c(CHL[j],CHU[j],CHU[j],CHL[j]), col=col.ci[i], border=FALSE)
        }
      }
    }
  }else{
    for(i in 1:ng){
      #this <- dat.list[[(ng+1)-i]]
      this <- subset(dat, group==group.names[i])
      x <- this$time ; CH <- this$cumHaz; CHL<-this$cumHazL; CHU<-this$cumHazU
      lines(x, CH, type='s', col=col.surv[i], lty=lty.surv[i], lwd=lwd.surv[i])
      if(no.ci==FALSE){
        lines(x, CHL, type='s', col=col.surv[i], lty=2, lwd=0.90)
        lines(x, CHU, type='s', col=col.surv[i], lty=2, lwd=0.90)
        for(j in 1:(length(x)-1)){
          polygon(x=c(x[j],x[j],x[j+1],x[j+1]), y=c(CHL[j],CHU[j],CHU[j],CHL[j]), col=col.ci[i], border=FALSE)
        }
      }
    }
  }
  box(bty=par('bty'))

  # par(op)
}


kmplot_vl <- function(km, cumHaz=FALSE, mark=3, simple=FALSE,
                      xaxis.at=pretty(km$time), xaxis.lab=xaxis.at,
                      lty.surv=1, lwd.surv=1, col.surv=1,
                      lty.ci=0, lwd.ci=1, col.ci=col.surv, #By default (lty.ci=0), confidence intervals are not plotted.
                      group.names=NULL, group.order=seq(length(km$n)), extra.left.margin=4,
                      label.n.at.risk=FALSE, draw.lines=TRUE, cex.axis=1,
                      xlab='', ylab='', main='', xlim=c(0,max(km$time)), ylim=c(0,1),
                      grid=TRUE, grid.vert=TRUE, lty.grid=1, lwd.grid=1, col.grid=grey(.9),
                      legend=!is.null(km$strata), loc.legend='topright', add=FALSE,
                      yaxis.at, yaxis.lab, leg.title, cex.text, add.zeroes=FALSE, no.ci=FALSE, cex.legend=1.0,
                      use.grp.4legend=TRUE, legend.names, extra.top=0,
                      move.lab.left=0, trunc.time.at, leg.ncol=1, vlines="PD",
                      ... # ... is passed to par()
){
  # Version 2.5.5: 2014/5/19

  # km is the output from survfit() function in survival package.

  # xaxis.at specifies where 'n at risk' will be computed and printed.
  # xaxis.lab specifies what will be printed at xaixs.at.  (see example)

  # If group names are long, add extra left margin by setting extra.left.margin to something greater than 0.

  # line specifications (lty.surv, lwd.surv, col.surv) will be recycled.
  # Set lty.ci to 1 if confidence intervals are needed.
  # group.names will overwrite whatever is specified in survfit() output.
  # group.order specifies the order of groups from top in 'n at risk'.  1 is at top, 2 next, and so on.

  # if add=TRUE, then par() is not refreshed.  allows multiple panels by
  # using, e.g., par(mfrow=c(2,2)).

  # op <- par(no.readonly = TRUE)

  ng0 <- length( km$strata ) ; ng <- max(ng0,1)
  # When only one group...
  if(ng0==0){ km$strata <- length(km$time) ; names(km$strata) <- 'All' ; legend <- draw.lines <- FALSE }

  lty.surv <- rep(lty.surv, ng) ; lwd.surv <- rep(lwd.surv, ng) ; col.surv <- rep(col.surv, ng)
  lty.ci <- rep(lty.ci, ng) ;     lwd.ci <- rep(lwd.ci, ng)     ; col.ci <- rep(col.ci, ng)

  ## group names and error checking
  gr <- c(km$strata)
  if( is.null(group.names) ){ group.names <- names(km$strata) }
  if( length(unique(group.names)) != ng ){ stop('\n','length(unique(group.names)) != number of groups.') }
  if( suppressWarnings(any( sort(group.order) != 1:ng)) )
  { stop('\n', 'Something wrong with group.order.','\n','sort(group.order) must equal 1:', ng, '.') }
  group.names <- gsub(' *$', '', group.names)  #to remove unwanted white spaces in group.names.
  if(ng==1 & (group.names[1]=='group.names') ){ group.names <- 'N at risk' ; label.n.at.risk = FALSE }

  ## graphic parameters
  if(!add){
    par(list(oma=c(1,1,1,1), mar=c(4+ng,4+extra.left.margin,1+extra.top,1)+.1))
    if(simple) par( mar=c(3,4,2,1)+.1 )
    par( list(...) )
  }else{
    par(mar=c(4+ng,4+extra.left.margin,1+extra.top,1)+.1)
  }

  ## reformat survival estimates
  dat <- data.frame(time=km$time, n.risk=km$n.risk, n.event=km$n.event, survival=km$surv, std.err=km$std.err,
                    lower=km$lower, upper=km$upper, group=rep( group.names, gr) )
  dat$cumHaz<-with(dat,(-1)*log(survival))
  dat$cumHazL<-with(dat, (-1)*log(lower))
  dat$cumHazU<-with(dat, (-1)*log(upper))
  if(is.na(trunc.time.at)==FALSE){
    dat<-subset(dat, time<=trunc.time.at)
  }
  dat.list <- split(dat, f=dat$group)

  ## plot (but not survival curves)
  plot(0,type='n', xlim=xlim, ylim=ylim, xaxt='n', yaxt='n', xlab='', ylab='', main=main)
  if(vlines=="PD"){
    abline(v=2.54, col="black")
    abline(v=4.48, col="black")
  }else if(vlines=="MCI"){
    abline(v=2.54, col="black")
    abline(v=4.38, col="black")
  }
  if(grid){
    par('xpd'=FALSE)
    if(grid.vert){
      abline(v=xaxis.at, lty=lty.grid, lwd=lwd.grid, col=col.grid)
    }
    abline(h=yaxis.at, lty=lty.grid, lwd=lwd.grid, col=col.grid )
  }
  axis( side=2, at=yaxis.at, label=yaxis.lab, cex.axis=cex.axis, las=1)
  axis( side=1, at=xaxis.at, label=xaxis.lab, line=-0.5, tick=FALSE, cex.axis=cex.axis )
  axis( side=1, at=xaxis.at, label=rep('',length(xaxis.at)), line=0, tick=TRUE )
  title(xlab=xlab, line=1.5, adj=.5, ...) ; title(ylab=ylab, ... )

  if(!simple){
    ## write group names
    group.name.pos <- (par()$usr[2]-par()$usr[1]) / -8 ; padding <- abs( group.name.pos / 8 )
    line.pos <- (1:ng)[order(group.order)] + 2
    if(use.grp.4legend){
      mtext( group.names, side=1, line=line.pos, at=group.name.pos+move.lab.left, adj=1, col=1, las=1, cex=cex.text)
    }else{
      mtext( legend.names, side=1, line=line.pos, at=group.name.pos+move.lab.left, adj=1, col=1, las=1, cex=cex.text)
    }

    ## draw matching lines for n at risk. [original is 0-2*padding, may need to change back]
    if(draw.lines){
      par('xpd'=TRUE)
      for(i in 1:ng){
        axis(side=1, at=c(group.name.pos+padding,0-2*padding)+move.lab.left, labels=FALSE, line=line.pos[i]+0.6, lwd.ticks=0,
             col=col.surv[i], lty=lty.surv[i], lwd=lwd.surv[i] ) }
    }

    ## numbers at risk
    kms <- summary(km, times=xaxis.at) ; if(is.null(kms$strata)) kms$strata <- rep(1,length(kms$time) )
    d1 <- data.frame(time = kms$time, n.risk = kms$n.risk, strata = c(kms$strata))
    d2 <- split(d1, f=d1$strata)

    ## Right-justifying the numbers
    ndigits <- lapply(d2, function(x) nchar(x[,2]) )
    max.len <- max( sapply(ndigits, length) )
    L <- do.call('rbind', lapply(ndigits, function(z){ length(z) <- max.len ; z} ) )
    nd <- apply( L, 2, max, na.rm=T )
    for( i in seq(ng) ){
      this <- d2[[i]]
      w.adj <- strwidth('0', cex=cex.axis, font=par('font')) / 2 * nd[1:nrow(this)]
      print(w.adj)
      mtext( side=1, at=this$time+w.adj, text=this$n.risk, line=line.pos[i], cex=cex.text, adj=1, col=1, las=1)
      if(add.zeroes){
        mtext(side=1, at=8+(w.adj[8]/2), text="0", line=line.pos[i], cex=cex.text, adj=1, col=1, las=1)
      }
    }
    if(label.n.at.risk) mtext(side=1, text='N at risk', at=group.name.pos, line=1.5, adj=1, col=1, las=1, cex=cex.text)
  } ## End of if(!simple)

  # Legend
  rlp <- group.order
  if(legend){
    bgc <- ifelse( par('bg')=='transparent', 'white', par('bg') )
    if(leg.title!=" "){
      if(use.grp.4legend){
        legend(x=loc.legend, legend=group.names[rlp], col=col.surv[rlp], lty=lty.surv[rlp], lwd=lwd.surv[rlp],
               bty='o', bg=bgc, ncol=leg.ncol, box.col='black', inset=.01, title=leg.title, cex=cex.legend)
      }else{
        legend(x=loc.legend, legend=legend.names[rlp], col=col.surv[rlp], lty=lty.surv[rlp], lwd=lwd.surv[rlp],
               bty='o', bg=bgc, ncol=leg.ncol, box.col='black', inset=.01, title=leg.title, cex=cex.legend)
      }
    }else{
      if(use.grp.4legend){
        legend(x=loc.legend, legend=group.names[rlp], col=col.surv[rlp], lty=lty.surv[rlp], lwd=lwd.surv[rlp],
               bty='o', bg=bgc, ncol=leg.ncol, box.col='black', inset=.01, cex=cex.legend)
      }else{
        legend(x=loc.legend, legend=legend.names[rlp], col=col.surv[rlp], lty=lty.surv[rlp], lwd=lwd.surv[rlp],
               bty='o', bg=bgc, ncol=leg.ncol, box.col='black', inset=.01, cex=cex.legend)
      }
    }
  }

  ## draw confidence intervals
  #if(no.ci==FALSE){
  #  for(i in 1:ng){
  #    this <- dat.list[[i]]
  #    x <- this$time ; L <- this$lower ; U <- this$upper ; S <- this$survival
  #    naL <- which( is.na(L) ) ; L[naL] <- L[naL-1] ; U[naL] <- U[naL-1]
  #    lines( x, L, type='s', col=col.ci[i], lty=lty.ci[i], lwd=lwd.ci[i] )
  #    lines( x, U, type='s', col=col.ci[i], lty=lty.ci[i], lwd=lwd.ci[i] )
  #  }
  #}

  # draw curves
  par(xpd=FALSE)
  if(cumHaz==FALSE){
    for(i in 1:ng){
      this <- subset(dat, group==group.names[i])
      x <- this$time ; CH <- this$survival; CHL<-this$lower; CHU<-this$upper
      lines(x, CH, type='s', col=col.surv[i], lty=lty.surv[i], lwd=lwd.surv[i])
      if(no.ci==FALSE){
        lines(x, CHL, type='s', col=col.surv[i], lty=2, lwd=0.90)
        lines(x, CHU, type='s', col=col.surv[i], lty=2, lwd=0.90)
        for(j in 1:(length(x)-1)){
          polygon(x=c(x[j],x[j],x[j+1],x[j+1]), y=c(CHL[j],CHU[j],CHU[j],CHL[j]), col=col.ci[i], border=FALSE)
        }
      }
    }
  }else{
    for(i in 1:ng){
      #this <- dat.list[[(ng+1)-i]]
      this <- subset(dat, group==group.names[i])
      x <- this$time ; CH <- this$cumHaz; CHL<-this$cumHazL; CHU<-this$cumHazU
      lines(x, CH, type='s', col=col.surv[i], lty=lty.surv[i], lwd=lwd.surv[i])
      if(no.ci==FALSE){
        lines(x, CHL, type='s', col=col.surv[i], lty=2, lwd=0.90)
        lines(x, CHU, type='s', col=col.surv[i], lty=2, lwd=0.90)
        for(j in 1:(length(x)-1)){
          polygon(x=c(x[j],x[j],x[j+1],x[j+1]), y=c(CHL[j],CHU[j],CHU[j],CHL[j]), col=col.ci[i], border=FALSE)
        }
      }
    }
  }
  box(bty=par('bty'))

  # par(op)
}
