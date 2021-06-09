## -----------------------------------------------------------------------------
library(rpmodel)
out_pmodel <- rpmodel( 
  tc             = 20,           # temperature, deg C
  vpd            = 1000,         # Pa,
  co2            = 400,          # ppm,
  fapar          = 1,            # fraction  ,
  ppfd           = 30,           # mol/m2/d,
  elv            = 0,            # m.a.s.l.,
  kphio          = 0.049977,     # quantum yield efficiency as calibrated for setup ORG by Stocker et al. 2020 GMD,
  beta           = 146,          # unit cost ratio a/b,
  c4             = FALSE,
  method_optci   = "prentice14",
  method_jmaxlim = "wang17",
  do_ftemp_kphio = FALSE,        # corresponding to setup ORG
  do_soilmstress = FALSE,        # corresponding to setup ORG
  verbose        = TRUE
  )
print(out_pmodel)

## ----eval=FALSE---------------------------------------------------------------
#  kphio = ifelse(do_ftemp_kphio, ifelse(do_soilmstress, 0.087182, 0.081785), 0.049977)
#  beta = 146.0
#  apar_soilm = 0.0
#  bpar_soilm = 0.73300

## -----------------------------------------------------------------------------
c_molmass <- 12.0107  # molecular mass of carbon
kphio <- 0.05         # quantum yield efficiency, value as used in the function call to rpmodel()
ppfd <- 30            # mol/m2/d, value as used in the function call to rpmodel()
fapar <- 1            # fraction, value as used in the function call to rpmodel()
print( out_pmodel$ci )
print( out_pmodel$ca - (out_pmodel$gpp / c_molmass) / out_pmodel$gs )
print( out_pmodel$ca * out_pmodel$chi )

## -----------------------------------------------------------------------------
print( out_pmodel$gpp / c_molmass )
print( out_pmodel$vcmax * (out_pmodel$ci - out_pmodel$gammastar) / (out_pmodel$ci + out_pmodel$kmm ))
print( out_pmodel$gs * (out_pmodel$ca - out_pmodel$ci) )

print( kphio * ppfd * fapar * (out_pmodel$ci - out_pmodel$gammastar) / (out_pmodel$ci + 2 * out_pmodel$gammastar ))

## -----------------------------------------------------------------------------
set.seed(1982)
out_ts_pmodel <- rpmodel( 
  tc             = 20 + rnorm(5, mean = 0, sd = 5),
  vpd            = 1000 + rnorm(5, mean = 0, sd = 50),
  co2            = rep(400, 5),
  fapar          = rep(1, 5),
  ppfd           = 30 + rnorm(5, mean = 0, sd = 3),
  elv            = 0,         
  kphio          = 0.049977,
  beta           = 146,
  c4             = FALSE,
  method_optci   = "prentice14",
  method_jmaxlim = "none",
  do_ftemp_kphio = TRUE,
  do_soilmstress = FALSE,
  verbose        = FALSE
  )
print(out_ts_pmodel$gpp)

## ----eval=FALSE---------------------------------------------------------------
#  library(dplyr)
#  library(purrr)
#  
#  set.seed(1982)
#  
#  df <- tibble(
#    tc             = 20 + rnorm(5, mean = 0, sd = 5),
#    vpd            = 1000 + rnorm(5, mean = 0, sd = 50),
#    co2            = rep(400, 5),
#    fapar          = rep(1, 5),
#    ppfd           = 30 + rnorm(5, mean = 0, sd = 3)
#    ) %>%
#    mutate( out_pmodel = purrr::pmap(., rpmodel,
#      elv            = 0,
#      kphio          = 0.049977,
#      beta           = 146,
#      c4             = FALSE,
#      method_optci   = "prentice14",
#      method_jmaxlim = "none",
#      do_ftemp_kphio = FALSE
#      ) )
#  
#  print(df)

## ----eval=FALSE---------------------------------------------------------------
#  library(tidyr)
#  df <- df %>%
#    mutate( out_pmodel = purrr::map(out_pmodel, ~as_tibble(.))) %>%
#    unnest(out_pmodel)
#  print(df)

## -----------------------------------------------------------------------------
out_pmodel <- rpmodel( 
  tc             = 10,           # temperature, deg C
  vpd            = 1000,         # Pa,
  co2            = 400,          # ppm,
  fapar          = 1,            # fraction  ,
  ppfd           = 30,           # mol/m2/d,
  elv            = 0,            # m.a.s.l.,
  kphio          = 0.049977,     # quantum yield efficiency as calibrated for setup ORG by Stocker et al. 2020 GMD,
  beta           = 146,          # unit cost ratio a/b,
  method_optci   = "prentice14",
  method_jmaxlim = "none",
  do_ftemp_kphio = FALSE,
  verbose        = TRUE
  )
print(paste("Ratio Vcmax/Vcmax25      :", out_pmodel$vcmax/out_pmodel$vcmax25))
print(paste("ftemp_inst_vcmax(10):", ftemp_inst_vcmax(10)))

## -----------------------------------------------------------------------------
print(paste("ftemp_inst_rd(10):", ftemp_inst_rd(10)))

## -----------------------------------------------------------------------------
print(paste("From rpmodel call :", out_pmodel$gammastar))
print(paste("gammastar(10):", gammastar(10, patm = patm(elv = 0))))

## -----------------------------------------------------------------------------
print(paste("From rpmodel call:", out_pmodel$kmm))
print(paste("kmm(10)     :", kmm(10, patm = patm(elv = 0))))

## -----------------------------------------------------------------------------
out_pmodel_ftemp_kphio_ON <- rpmodel( 
  tc             = 20,           # temperature, deg C
  vpd            = 1000,         # Pa,
  co2            = 400,          # ppm,
  fapar          = 1,            # fraction  ,
  ppfd           = 30,           # mol/m2/d,
  elv            = 0,            # m.a.s.l.,
  do_ftemp_kphio = TRUE
  )
out_pmodel_ftemp_kphio_OFF <- rpmodel( 
  tc             = 20,           # temperature, deg C
  vpd            = 1000,         # Pa,
  co2            = 400,          # ppm,
  fapar          = 1,            # fraction  ,
  ppfd           = 30,           # mol/m2/d,
  elv            = 0,            # m.a.s.l.,
  do_ftemp_kphio = FALSE
  )
print(paste("LUE ftemp_ON /LUE ftemp_OFF =", out_pmodel_ftemp_kphio_ON$lue / out_pmodel_ftemp_kphio_OFF$lue))
print(paste("GPP ftemp_ON /GPP ftemp_OFF =", out_pmodel_ftemp_kphio_ON$gpp / out_pmodel_ftemp_kphio_OFF$gpp))
print(paste("Vcmax ftemp_ON /Vcmax ftemp_OFF =", out_pmodel_ftemp_kphio_ON$vcmax / out_pmodel_ftemp_kphio_OFF$vcmax))
print(paste("ftemp_kphio(20) =", ftemp_kphio(20)))

## -----------------------------------------------------------------------------
vec_soilm <- seq(from = 1.0, to = 0.0, by = -0.05)
vec_soilmstress <- soilmstress( vec_soilm, meanalpha = 1.0, apar_soilm = 0.0, bpar_soilm = 0.7330 )
plot(vec_soilm, vec_soilmstress)

## -----------------------------------------------------------------------------
out_pmodel_soilmstress_OFF <- rpmodel( 
  tc             = 20,           # temperature, deg C
  vpd            = 1000,         # Pa,
  co2            = 400,          # ppm,
  fapar          = 1,            # fraction  ,
  ppfd           = 30,           # mol/m2/d,
  elv            = 0,            # m.a.s.l.,
  do_ftemp_kphio = FALSE,
  do_soilmstress = FALSE
  )
out_pmodel_soilmstress_ON <- rpmodel( 
  tc             = 20,           # temperature, deg C
  vpd            = 1000,         # Pa,
  co2            = 400,          # ppm,
  fapar          = 1,            # fraction  ,
  ppfd           = 30,           # mol/m2/d,
  elv            = 0,            # m.a.s.l.,
  do_ftemp_kphio = FALSE,
  do_soilmstress = TRUE,
  soilm          = 0.2,
  apar_soilm     = 0.1,
  bpar_soilm     = 0.7,
  meanalpha      = 0.2 
  )
print(paste("LUE soilmstress_ON /LUE soilmstress_OFF =", out_pmodel_soilmstress_ON$lue / out_pmodel_soilmstress_OFF$lue))
print(paste("GPP soilmstress_ON /GPP soilmstress_OFF =", out_pmodel_soilmstress_ON$gpp / out_pmodel_soilmstress_OFF$gpp))
print(paste("Vcmax soilmstress_ON /Vcmax soilmstress_OFF =", out_pmodel_soilmstress_ON$vcmax / out_pmodel_soilmstress_OFF$vcmax))
print(paste("soilmstress(0.2, apar_soilm = 0.1, bpar_soilm = 0.7, meanalpha = 0.2) =", soilmstress(0.2, apar_soilm = 0.1, bpar_soilm = 0.7, meanalpha = 0.2)))

