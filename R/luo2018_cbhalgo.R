# Implements CBH (crown base height) estimation for tree LIDAR/.las data.
# It's specified in the paper:

# Laiping Luo, Qiuping Zhai, Yanjun Su, 
# Qin Ma, Maggi Kelly, and Qinghua Guo, 
# "Simple method for direct crown base height estimation of individual conifer trees using airborne LiDAR data," 
# Opt. Express 26, 
# A562-A578 
# (2018) 

# available at time of writing from:
# https://www.osapublishing.org/oe/fulltext.cfm?uri=oe-26-10-A562&id=385987

# this derivative work is provided as public domain.

library(TreeLS)
library(pracma)
library(equate)

# # Example on how to read treelas input:
# treelas = readTLS(paste(filenamewithpath,".las",sep=""))

###################################################
################UTILITY FUNCTIONS##################
###################################################

bringToOrigin = function(las, ...){
  las@data$X = las@data$X - min(las@data$X)
  las@data$Y = las@data$Y - min(las@data$Y)
  las@data$Z = las@data$Z - min(las@data$Z)
  return(las)
}

# A point density estimate that mimicks LASTools' command:
# lasinfo mylas.las -compute_density
# (although not in all cases exactly)
# see: https://gis.stackexchange.com/questions/416426/why-do-lidr-and-lastools-report-different-point-densities
point_density = function(las, ...){
  d = grid_density(las,2)
  return(mean(d[d>0]))
}

###################################################
###############CBH ESTIMATION ALGO#################
###################################################

luo2018cbh = function(treelas, pr_cutoff=0.3, plotting=FALSE) {

  tls = bringToOrigin(treelas)
  
  if(plotting == TRUE) {
    par(mfrow=c(1,2))
  }
  
  if(plotting == TRUE) {
    x=plot(tls, title=filename,color = "Z", colorPalette="white")
    rgl::title3d("","",'X', 'Y', 'Z', col='white')
    rgl::axes3d(c('x', 'y', 'z'),col='white')
  }

  ###################################################
  ###############UNDERSTORY PART (p. 8)##############
  ###################################################
  
  # slice the tree cloud vertically to slices of height dh
  
  dh = 0.1 # height interval size (also an accuracy parameter)
  
  # construct a vertical histogram of number of points per height segment
  # into freqs vector
  
  freqs = numeric(0)
  slice_max = ceiling(max(tls@data$Z))
  hseq = seq(0,slice_max,dh)
  for(i in hseq) {
    freqs[length(freqs)+1]=length(filter_poi(tls, tls@data$Z > i & tls@data$Z <= i+dh)@data$X)
  }
  
  # For freqs data, construct:
  #   -spline interpolation
  #   -2nd derivative for spline
  
  fit <- smooth.spline(hseq, freqs, spar=0.1) # or 0.1
  pred <- function(x) stats:::predict.smooth.spline(fit, x)$y
  pred1stderiv <- function(x)  stats:::predict.smooth.spline(fit, x, deriv=1)$y
  pred2ndderiv <- function(x)  stats:::predict.smooth.spline(fit, x, deriv=2)$y
  
  # find roots of 2nd derivative curve
  ddzeros = findzeros(pred2ndderiv,0,slice_max,n=(slice_max)/dh)
  # these are critical points of 1st derivative
  d_at_zeros = pred1stderiv(ddzeros)
  
  if(plotting == TRUE) {
    # visualize all critical points and positive parts of 2nd derivative, p. 9 paper
    plot(hseq,pred2ndderiv(hseq),type = "l", xlab="height (m)",main="Understory: 2nd derivative and critical points.
         Red/Green: start/end of pos. parts.")
    points(ddzeros, pred2ndderiv(ddzeros), pch=1)
  }
  
  # select intervals between which the 2nd derivative is positive
  pos_d2intervals_s = numeric(0) # [i] will give start point ith positive interval
  pos_d2intervals_e = numeric(0) # [i] will give end point of ith positive interval
  
  for (i in 2:length(ddzeros)) {
    d2_at_interval = pred2ndderiv(seq(ddzeros[i-1],ddzeros[i],0.1))
    # is d2 positive on this interval?
    if (sum(d2_at_interval) > 0.0) {
      
      pos_d2intervals_s[length(pos_d2intervals_s)+1] = ddzeros[i-1]
      pos_d2intervals_e[length(pos_d2intervals_e)+1] = ddzeros[i]
      
      if(plotting == TRUE) {
        points(pos_d2intervals_s, pos_d2intervals_s*0+max(pred2ndderiv(hseq))/2,pch=20,col="red")
        points(pos_d2intervals_e, pos_d2intervals_s*0+max(pred2ndderiv(hseq))/2,pch=20,col="green")
      }
    }
  }
  
  # now we need to compute point density of input point cloud on
  # each of these positive intervals/slices
  
  slicedensities = numeric(0)
  for (i in 1:length(pos_d2intervals_s)) {
    slice = filter_poi(tls, tls@data$Z > pos_d2intervals_s[i] & tls@data$Z <= pos_d2intervals_e[i])
    if (length(slice@data$X)>0.0) {
      slicedensities[length(slicedensities)+1] = point_density(slice)
    } else { # except for possible erroneous 0 point intervals
      slicedensities[length(slicedensities)+1] = NA
    }
  }
  
  # the end of understory vegetation should be at the lower point of interval of lowest density
  # (again, p. 9 in paper)
  understory_min = pos_d2intervals_s[which(slicedensities == min(slicedensities,na.rm=TRUE))]
  
  if(plotting == TRUE) {
    rgl::rgl.points(0,0,understory_min,col='green',size=30)
  }
  
  withoutunderstory = filter_poi(tls, tls@data$Z > understory_min)
  
  ###################################################
  ##################CBH PART (p. 9)##################
  ###################################################
  
  dh = 0.1 # height interval size (accuracy parameter)
  
  freqs = numeric(0)
  slice_min = round(min(withoutunderstory@data$Z),digits=1)
  slice_max = round(max(withoutunderstory@data$Z), digits=1)+dh
  
  # construct a vertical histogram of the tree
  hseq = seq(slice_min,slice_max-dh,dh)
  for(i in hseq) {
    freqs[length(freqs)+1]=length(filter_poi(withoutunderstory, withoutunderstory@data$Z > i & withoutunderstory@data$Z < i+dh)@data$X)
  }
  
  # PercentRank of freqs
  densities = px(freqs)
  
  fit <- smooth.spline(hseq, densities, spar=0.5)
  pred <- function(x) stats:::predict.smooth.spline(fit, x)$y
  pred1stderiv <- function(x) stats:::predict.smooth.spline(fit, x, deriv=1)$y
  pred2ndderiv <- function(x)  stats:::predict.smooth.spline(fit, x, deriv=2)$y
  
  ddzeros = findzeros(pred2ndderiv,slice_min,slice_max,n=ceiling((slice_max)/dh))
  
  # PercentRank cut-off point (pr_cutoff):
  #   should be chosen relative to the point cloud accuracy
  #   or e.g. different values for different tree species.
  
  pr_cutoff = pr_cutoff
  
  smallhseq = hseq[densities < pr_cutoff]
  
  smallddzeros = ddzeros[which(ddzeros < smallhseq[length(smallhseq)])]
  
  #####################EXCEPTION######################
  # in case such points would not exist, then we could
  # not improve from understory estimate
  if(length(smallddzeros) == 0) {
    smallddzeros = understory_min
  }
  ####################################################
  
  d_at_zeros = pred1stderiv(smallddzeros)
  
  if(plotting == TRUE) {
    # mimicks plot at p. 10
    plot(hseq,pred(hseq),type="l",xlab="height (m)", ylab="PercentRank spline",main="CBH: black (percent rank), blue (1st deriv),
          red (2nd deriv), green (pr_cutoff)")
    lines(hseq,pred1stderiv(hseq),type='l',col="cyan")
    lines(smallhseq,pred1stderiv(smallhseq),col="blue")
    lines(smallhseq,pred2ndderiv(smallhseq),col="red")
    points(smallddzeros,pred2ndderiv(smallddzeros),col="red")
    abline(h=pr_cutoff, col="green")
  }
  
  cbh = ddzeros[which(d_at_zeros == max(d_at_zeros, na.rm=TRUE))]
  
  if(plotting == TRUE) {
    points(cbh, 0.0, col="red",pch=20)
    rgl::rgl.points(0,0,cbh,col='red',size=20)
  }
  
  return(cbh)
}

# luo2018cbh(treelas,pr_cutoff=0.3,plotting=TRUE)