allMetrics = function(x, y, z, i, angle, return.number, number.return, class, pulseID, flightlineID, canopyResolution = 2, vciThreshold = 45, layerThickness = 1)
{
  xmax = max(x)
  ymax = max(y)
  xmin = min(x)
  ymin = min(y)
  zmax = max(z)
  n    = length(z)
  abs.angle = abs(angle)

  # =====================
  # Variables for control
  # =====================

  A 		  = (xmax - xmin)*(ymax - ymin)
  dpoint 	= n/A
  dpulse 	= length(unique(pulseID))/A
  angle 	= mean(abs.angle)
  anglesd = sd(abs.angle)

  nline = flightlineID %>% unique %>% length

  # ================================
  # Metrics from Z elevation
  # ================================

  hq 	  = as.numeric(quantile(z, seq(0.1, 0.9, 0.2)))

  hmax  = zmax
  hmean = mean(z)
  hsd   = sd(z)
  hcv   = hsd/hmean

  hq10  = hq[1]
  hq30  = hq[2]
  hq50  = hq[3]
  hq70  = hq[4]
  hq90  = hq[5]

  hskew = (sum((z - hmean)^3)/n)/(sum((z - hmean)^2)/n)^(3/2)
  hkurt = n * sum((z - hmean)^4)/(sum((z - hmean)^2)^2)

  # =================================
  # Metrics from intensity
  # =================================

  iq   = as.numeric(quantile(i, seq(.1, .9, 0.2)))
  itot = sum(i)

  # Classical metrics
  imax  = max(i)
  imean = mean(i)
  isd   = sd(i)
  icv   = isd/imean

  # Intensity percentiles
  iq10 = iq[1]
  iq30 = iq[2]
  iq50 = iq[3]
  iq70 = iq[4]
  iq90 = iq[5]

  # Perrcentage of intensity returned by ground
  ing = sum(i[class == 2])

  # Cumulated intensity at a given percentile of height
  ih10 = sum(i[z <= hq10])
  ih30 = sum(i[z <= hq30])
  ih50 = sum(i[z <= hq50])
  ih70 = sum(i[z <= hq70])
  ih90 = sum(i[z <= hq90])

  # Intensity returned by the ith returns
  in1 = sum(i[return.number == 1])
  in2 = sum(i[return.number == 2])
  in3 = sum(i[return.number == 3])
  in4 = sum(i[return.number == 4])

  iskew = (sum((i - imean)^3)/n)/(sum((i - imean)^2)/n)^(3/2)
  ikurt = n * sum((i - imean)^4)/(sum((i - imean)^2)^2)

  # =======================================
  # Metrics from classification
  # =======================================

  rn = table(factor(return.number, levels=c(0:4)))
  cl = table(factor(class, levels=c(0:2)))

  # Number of ith returns
  n1 = rn[2]
  n2 = rn[3]
  n3 = rn[4]
  n4 = rn[5]

  # Number of return from ground
  ng = cl[3]

  # Number of first returns from ground
  n1g = sum(return.number == 1 & class == 2)

  perc.ng  = ng/n*100
  perc.n1  = n1/n*100
  perc.n2  = n2/n*100
  perc.n3  = n3/n*100
  perc.n4  = n4/n*100
  perc.n1g = n1g/n1*100

  # =========================
  # Metric from canopy
  # =========================

  canopy = canopyMatrix(x,y,z, canopyResolution)

  canopyq    = as.numeric(quantile(canopy, seq(0.1, 0.9, 0.2), na.rm=T))

  canopymean = mean(canopy, na.rm=T)
  canopysd   = sd(canopy, na.rm=T)
  canopycv   = canopysd/canopymean

  canopyq10  = canopyq[1]
  canopyq30  = canopyq[2]
  canopyq50  = canopyq[3]
  canopyq70  = canopyq[4]
  canopyq90  = canopyq[5]

  canopyfd   = fractal.dimension(canopy)

  perc.canopy.na = sum(is.na(canopy))/length(canopy) * 100

  CC2  = sum(canopy > 2, na.rm=T)/length(canopy)*100
  CC10 = sum(canopy > 10, na.rm=T)/length(canopy)*100
  CC20 = sum(canopy > 20, na.rm=T)/length(canopy)*100
  CC30 = sum(canopy > 30, na.rm=T)/length(canopy)*100
  CC40 = sum(canopy > 40, na.rm=T)/length(canopy)*100

  # =============================
  # Metric from pulses
  # =============================

  t = perc_pulses_with_n_returns(return.number, pulseID)

  # Number of pulses with i returns
  np1 = t[1]
  np2 = t[2]
  np3 = t[3]
  np4 = t[4]

  # Percentage au pulse with i returns
  perc.np1 = np1/n*100
  perc.np2 = np2/n*100
  perc.np3 = np3/n*100
  perc.np4 = np4/n*100

  # ====================================
  # Metrics of penetration
  # ====================================

  # percentage of return reaching height < 2m (Bouvier et al. 2015)
  pf = length(z < 2)/n*100

  z[z<0] = 0
  hs  = entropy(z, layerThickness)
  vci = entropy(z, zmax = vciThreshold, layerThickness)
  lad = LAD(z, by = layerThickness)

  lad.hmax = as.numeric(which.max(lad))
  lad.max = max(lad)
  lad.mean = mean(lad)
  lad.cv = sd(lad)/mean(lad) #(Bouvier et al. 2015)

  ret = list(
    A = A,
    npoints = n,
    dpoint = dpoint,
    dpulse = dpulse,
    angle  = angle,
    anglesd = anglesd,
    perc.canopy.na = perc.canopy.na,
    nline = nline,

    hmax  = hmax,
    hmean = hmean,
    hsd   = hsd,
    hcv   = hcv,
    hs    = hs,
    vci   = vci,

    hq10  = hq10,
    hq30  = hq30,
    hq50  = hq50,
    hq70  = hq70,
    hq90  = hq90,

    hskew = hskew,
    hkurt = hkurt,

    itot = itot,
    imax = imax,
    imean = imean,
    isd  = isd,
    icv  = icv,
    iq10 = iq10,
    iq30 = iq30,
    iq50 = iq50,
    iq70 = iq70,
    iq90 = iq90,
    ing  = ing,
    ih10 = ih10,
    ih30 = ih30,
    ih50 = ih50,
    ih70 = ih70,
    ih90 = ih90,
    in1  = in1,
    in2  = in2,
    in3  = in3,
    in4  = in4,
    iskew = iskew,
    ikurt = ikurt,

    perc.ng  = perc.ng,
    perc.n1  = perc.n1,
    perc.n2  = perc.n2,
    perc.n3  = perc.n3,
    perc.n4  = perc.n4,
    perc.n1g = perc.n1g,

    canopymean =canopymean,
    canopysd   =canopysd,
    canopycv   =canopycv,
    canopyq10  =canopyq10,
    canopyq30  =canopyq30,
    canopyq50  =canopyq50,
    canopyq70  =canopyq70,
    canopyq90  =canopyq90,
    canopyfd   =canopyfd,

    CC2 = CC2,
    CC10 = CC10,
    CC20 = CC20,
    CC30 = CC30,
    CC40 = CC40,

    perc.np1 = perc.np1,
    perc.np2 = perc.np2,
    perc.np3 = perc.np3,
    perc.np4 = perc.np4,

    pf = pf,

    lad.hmax = lad.hmax,
    lad.max  = lad.max,
    lad.mean = lad.mean,
    lad.cv   = lad.cv
  )

  return(lapply(ret, round, digits=3))
}