# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
#
# This file is part of lidR R package.
#
# lidR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>
#
# ===============================================================================



metricslist = list(
  "HMAX"     = "max(Z)",
  "HMEAN"    = "mean(Z)",
  "HSD"      = "sd(Z)",
  "HCV"      = "sd(Z)/mean(Z)",
  "HQ05"     = "quantile(Z, 0.05)",
  "HQ10"     = "quantile(Z, 0.1)",
  "HQ20"     = "quantile(Z, 0.2)",
  "HQ30"     = "quantile(Z, 0.3)",
  "HQ40"     = "quantile(Z, 0.4)",
  "HQ50"     = "quantile(Z, 0.5)",
  "HQ60"     = "quantile(Z, 0.6)",
  "HQ70"     = "quantile(Z, 0.7)",
  "HQ80"     = "quantile(Z, 0.80)",
  "HQ90"     = "quantile(Z, 0.9)",
  "HQ95"     = "quantile(Z, 0.95)",
  "HQ99"     = "quantile(Z, 0.99)",
  "HSKEW"    = "(sum((Z - mean(Z))^3)/length(Z))/(sum((Z - mean(Z))^2)/length(Z))^(3/2)",
  "HKURT"    = "length(Z) * sum((Z - mean(Z))^4)/(sum((Z - mean(Z))^2)^2)",

  "ITOT"     = "sum(Intensity)",
  "IMAX"     = "max(Intensity)",
  "IMEAN"    = "mean(Intensity)",
  "ISD"      = "sd(Intensity)",
  "ICV"      = "sd(Intensity)/mean(Intensity)",
  "IQ05"     = "quantile(Intensity, 0.05)",
  "IQ10"     = "quantile(Intensity, 0.1)",
  "IQ20"     = "quantile(Intensity, 0.2)",
  "IQ30"     = "quantile(Intensity, 0.3)",
  "IQ40"     = "quantile(Intensity, 0.4)",
  "IQ50"     = "quantile(Intensity, 0.5)",
  "IQ60"     = "quantile(Intensity, 0.6)",
  "IQ70"     = "quantile(Intensity, 0.7)",
  "IQ80"     = "quantile(Intensity, 0.80)",
  "IQ90"     = "quantile(Intensity, 0.9)",
  "IQ95"     = "quantile(Intensity, 0.95)",
  "IQ99"     = "quantile(Intensity, 0.99)",
  "IGROUND"  = "sum(Intensity[Classification == 2])",
  "I1PERC"   = "sum(Intensity[ReturnNumber == 1])/sum(Intensity)",
  "I2PERC"   = "sum(Intensity[ReturnNumber == 2])/sum(Intensity)",
  "I3PERC"   = "sum(Intensity[ReturnNumber == 3])/sum(Intensity)",
  "I5PERC"   = "sum(Intensity[ReturnNumber == 4])/sum(Intensity)",
  "I5PERC"   = "sum(Intensity[ReturnNumber == 5])/sum(Intensity)",
  "ISKEW"    = "(sum((Intensity - mean(Intensity))^3)/length(Intensity))/(sum((Intensity - mean(Intensity))^2)/length(Intensity))^(3/2)",
  "IKURT"    = "length(Intensity) * sum((Intensity - mean(Intensity))^4)/(sum((Intensity - mean(Intensity))^2)^2)",

  "N1"       = "sum(ReturnNumber == 1)",
  "N2"       = "sum(ReturnNumber == 2)",
  "N3"       = "sum(ReturnNumber == 3)",
  "N4"       = "sum(ReturnNumber == 4)",
  "N5"       = "sum(ReturnNumber == 5)",
  "NGROUND"  = "sum(Classification == 2)",
  "N1GROUND" = "sum(Classification == 2 & ReturnNumber == 1)",

  "N1PERC"   = "sum(ReturnNumber == 1)/length(Z)*100",
  "N2PERC"   = "sum(ReturnNumber == 2)/length(Z)*100",
  "N3PERC"   = "sum(ReturnNumber == 3)/length(Z)*100",
  "N4PERC"   = "sum(ReturnNumber == 4)/length(Z)*100",
  "N5PERC"   = "sum(ReturnNumber == 5)/length(Z)*100",
  "NGROUNDPERC" = "sum(Classification == 2)/length(Z)*100",
  "N1GROUNDPERC"= "sum(Classification == 2 & ReturnNumber == 1)/length(Z)*100",

  "PERCSINGLE"  = "perc_pulses_with_n_returns(ReturnNumber, pulseID)[1]",
  "PERC1RETURN" = "perc_pulses_with_n_returns(ReturnNumber, pulseID)[1]",
  "PERC2RETURN" = "perc_pulses_with_n_returns(ReturnNumber, pulseID)[2]",
  "PERC3RETURN" = "perc_pulses_with_n_returns(ReturnNumber, pulseID)[3]",
  "PERC4RETURN" = "perc_pulses_with_n_returns(ReturnNumber, pulseID)[4]",
  "PERC5RETURN" = "perc_pulses_with_n_returns(ReturnNumber, pulseID)[5]",

  "PERC_LOWER_1m"= "sum(Z < 1)/length(Z)*100",
  "PERC_LOWER_2m"= "sum(Z < 2)/length(Z)*100",
  "PERC_LOWER_3m"= "sum(Z < 3)/length(Z)*100",
  "PERC_LOWER_5m"= "sum(Z < 5)/length(Z)*100",

  "ENTROPY"    = "entropy(Z)",
  "VCI"        = "entropy(Z, zmax = threshold)",
  "LAD"        = "LAD(Z)",
  "LADCV"      = "sd(LAD(Z))/mean(LAD(Z))",

  "CANOPY_MEAN" = "mean(local_maximum(X,Y,Z, resolution), na.rm=TRUE)",
  "CANOPY_SD" = "sd(local_maximum(X,Y,Z, resoluion), na.rm=TRUE)",
  "CANOPY_FD"  = "fractal_dimension(local_maximum(X, Y, Z, resolution))",
  "CANOPYCLOSURE" = "canopy_closure(X, Y, Z, resolution, threshold)",

  "NPULSE" = "length(unique(pulseID))"
)


