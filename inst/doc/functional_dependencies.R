## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(dplyr)
library(ggplot2)
library(rpmodel)

## -----------------------------------------------------------------------------
out_ts <- rpmodel::rpmodel( 
  tc             = seq(10, 30, by = 1),
  vpd            = 300,
  co2            = 400,
  fapar          = 1,
  ppfd           = 30,
  elv            = 0,         
  c4             = FALSE,
  do_ftemp_kphio = FALSE,
  do_soilmstress = FALSE
  ) %>% 
  as_tibble() %>% 
  mutate(setup = "ORG",
             tc = seq(10, 30, by = 1)) %>% 
  bind_rows(
    rpmodel::rpmodel( 
      tc             = seq(10, 30, by = 1),
      vpd            = 300,
      co2            = 400,
      fapar          = 1,
      ppfd           = 30,
      elv            = 0,         
      c4             = FALSE,
      do_ftemp_kphio = TRUE,
      do_soilmstress = FALSE
      ) %>% 
      as_tibble() %>% 
      mutate(setup = "BRC",
             tc = seq(10, 30, by = 1))
      )

out_ts %>% 
  ggplot(aes(x = tc, y = gpp, color = setup)) +
  geom_line() +
  labs(x = "Temperature (deg C)", y = "GPP (g m-2 d-1)")

out_ts %>% 
  ggplot(aes(x = tc, y = chi, color = setup)) +
  geom_line() +
  labs(x = "Temperature (deg C)", y = "chi")

out_ts %>% 
  ggplot(aes(x = tc, y = vcmax, color = setup)) +
  geom_line() +
  labs(x = "Temperature (deg C)", y = "Vcmax (g m-2 d-1)")

## -----------------------------------------------------------------------------
out_ts <- rpmodel::rpmodel( 
  tc             = 20,
  vpd            = seq(0, 4000, by = 50),
  co2            = 400,
  fapar          = 1,
  ppfd           = 30,
  elv            = 0,         
  c4             = FALSE,
  do_ftemp_kphio = FALSE,
  do_soilmstress = FALSE
  ) %>% 
  as_tibble() %>% 
  mutate(setup = "ORG",
             vpd = seq(0, 4000, by = 50)) %>% 
  bind_rows(
    rpmodel::rpmodel( 
      tc             = 20,
      vpd            = seq(0, 4000, by = 50),
      co2            = 400,
      fapar          = 1,
      ppfd           = 30,
      elv            = 0,         
      c4             = FALSE,
      do_ftemp_kphio = TRUE,
      do_soilmstress = FALSE
      ) %>% 
      as_tibble() %>% 
      mutate(setup = "BRC",
             vpd = seq(0, 4000, by = 50))
      )

out_ts %>% 
  ggplot(aes(x = vpd, y = gpp, color = setup)) +
  geom_line() +
  labs(x = "VPD (Pa)", y = "GPP (g m-2 d-1)")

out_ts %>% 
  ggplot(aes(x = vpd, y = chi, color = setup)) +
  geom_line() +
  labs(x = "VPD (Pa)", y = "chi")

out_ts %>% 
  ggplot(aes(x = vpd, y = vcmax, color = setup)) +
  geom_line() +
  labs(x = "VPD (Pa)", y = "Vcmax (g m-2 d-1)")

## -----------------------------------------------------------------------------
out_ts <- rpmodel::rpmodel( 
  tc             = 20,
  vpd            = 300,
  co2            = 400,
  fapar          = 1,
  ppfd           = seq(1, 50, by = 1),
  elv            = 0,         
  c4             = FALSE,
  do_ftemp_kphio = FALSE,
  do_soilmstress = FALSE
  ) %>% 
  as_tibble() %>% 
  mutate(setup = "ORG",
         ppfd = seq(1, 50, by = 1)) %>% 
  bind_rows(
    rpmodel::rpmodel( 
      tc             = 20,
      vpd            = 300,
      co2            = 400,
      fapar          = 1,
      ppfd           = seq(1, 50, by = 1),
      elv            = 0,         
      c4             = FALSE,
      do_ftemp_kphio = TRUE,
      do_soilmstress = FALSE
      ) %>% 
      as_tibble() %>% 
      mutate(setup = "BRC",
             ppfd = seq(1, 50, by = 1))
      )

out_ts %>% 
  ggplot(aes(x = ppfd, y = gpp, color = setup)) +
  geom_line() +
  labs(x = "PPFD (mol m-2 d-1)", y = "GPP (g m-2 d-1)")

out_ts %>% 
  ggplot(aes(x = ppfd, y = chi, color = setup)) +
  geom_line() +
  labs(x = "PPFD (mol m-2 d-1)", y = "chi")

out_ts %>% 
  ggplot(aes(x = ppfd, y = vcmax, color = setup)) +
  geom_line() +
  labs(x = "PPFD (mol m-2 d-1)", y = "Vmax (g m-2 d-1)")

## -----------------------------------------------------------------------------
calc_patm <- function( elv, patm0 = 101325 ){
  
  # Define constants:
  kTo <- 298.15    # base temperature, K (Prentice, unpublished)
  kL  <- 0.0065    # adiabiatic temperature lapse rate, K/m (Allen, 1973)
  kG  <- 9.80665   # gravitational acceleration, m/s^2 (Allen, 1973)
  kR  <- 8.3145    # universal gas constant, J/mol/K (Allen, 1973)
  kMa <- 0.028963  # molecular weight of dry air, kg/mol (Tsilingiris, 2008)
  
  # Convert elevation to pressure, Pa:
  patm <- patm0*(1.0 - kL*elv/kTo)^(kG*kMa/(kR*kL))
  
  return(patm)
}

out_ts <- rpmodel::rpmodel( 
  tc             = 20,
  vpd            = 300,
  co2            = 400,
  fapar          = 1,
  ppfd           = 30,
  patm           = calc_patm(seq(0, 4000, by = 40)),         
  c4             = FALSE,
  do_ftemp_kphio = FALSE,
  do_soilmstress = FALSE
  ) %>% 
  as_tibble() %>% 
  mutate(setup = "ORG",
         patm = calc_patm(seq(0, 4000, by = 40))) %>% 
  bind_rows(
    rpmodel::rpmodel( 
      tc             = 20,
      vpd            = 300,
      co2            = 400,
      fapar          = 1,
      ppfd           = 30,
      patm           = calc_patm(seq(0, 4000, by = 40)),         
      c4             = FALSE,
      do_ftemp_kphio = TRUE,
      do_soilmstress = FALSE
      ) %>% 
      as_tibble() %>% 
      mutate(setup = "BRC",
             patm = calc_patm(seq(0, 4000, by = 40)))
      )

out_ts %>% 
  ggplot(aes(x = patm, y = gpp, color = setup)) +
  geom_line() +
  labs(x = "P (Pa)", y = "GPP (g m-2 d-1)")

out_ts %>% 
  ggplot(aes(x = patm, y = chi, color = setup)) +
  geom_line() +
  labs(x = "P (Pa)", y = "chi")

out_ts %>% 
  ggplot(aes(x = patm, y = vcmax, color = setup)) +
  geom_line() +
  labs(x = "P (Pa)", y = "Vcmax (g m-2 d-1)")

## -----------------------------------------------------------------------------
out_ts <- rpmodel::rpmodel( 
  tc             = seq(10, 30, by = 1),
  vpd            = 300,
  co2            = 400,
  fapar          = 1,
  ppfd           = 30,
  elv            = 0,         
  c4             = TRUE,
  do_ftemp_kphio = FALSE,
  do_soilmstress = FALSE
  ) %>% 
  as_tibble() %>% 
  mutate(setup = "ORG",
             tc = seq(10, 30, by = 1)) %>% 
  bind_rows(
    rpmodel::rpmodel( 
      tc             = seq(10, 30, by = 1),
      vpd            = 300,
      co2            = 400,
      fapar          = 1,
      ppfd           = 30,
      elv            = 0,         
      c4             = TRUE,
      do_ftemp_kphio = TRUE,
      do_soilmstress = FALSE
      ) %>% 
      as_tibble() %>% 
      mutate(setup = "BRC",
             tc = seq(10, 30, by = 1))
      )

out_ts %>% 
  ggplot(aes(x = tc, y = gpp, color = setup)) +
  geom_line() +
  labs(x = "Temperature (deg C)", y = "GPP (g m-2 d-1)")

out_ts %>% 
  ggplot(aes(x = tc, y = chi, color = setup)) +
  geom_line() +
  labs(x = "Temperature (deg C)", y = "chi")

out_ts %>% 
  ggplot(aes(x = tc, y = vcmax, color = setup)) +
  geom_line() +
  labs(x = "Temperature (deg C)", y = "Vcmax (g m-2 d-1)")

## -----------------------------------------------------------------------------
out_ts <- rpmodel::rpmodel( 
  tc             = 20,
  vpd            = seq(0, 4000, by = 50),
  co2            = 400,
  fapar          = 1,
  ppfd           = 30,
  elv            = 0,         
  c4             = TRUE,
  do_ftemp_kphio = FALSE,
  do_soilmstress = FALSE
  ) %>% 
  as_tibble() %>% 
  mutate(setup = "ORG",
             vpd = seq(0, 4000, by = 50)) %>% 
  bind_rows(
    rpmodel::rpmodel( 
      tc             = 20,
      vpd            = seq(0, 4000, by = 50),
      co2            = 400,
      fapar          = 1,
      ppfd           = 30,
      elv            = 0,         
      c4             = TRUE,
      do_ftemp_kphio = TRUE,
      do_soilmstress = FALSE
      ) %>% 
      as_tibble() %>% 
      mutate(setup = "BRC",
             vpd = seq(0, 4000, by = 50))
      )

out_ts %>% 
  ggplot(aes(x = vpd, y = gpp, color = setup)) +
  geom_line() +
  labs(x = "VPD (Pa)", y = "GPP (g m-2 d-1)")

out_ts %>% 
  ggplot(aes(x = vpd, y = chi, color = setup)) +
  geom_line() +
  labs(x = "VPD (Pa)", y = "chi")

out_ts %>% 
  ggplot(aes(x = vpd, y = vcmax, color = setup)) +
  geom_line() +
  labs(x = "VPD (Pa)", y = "Vcmax (g m-2 d-1)")

## -----------------------------------------------------------------------------
out_ts <- rpmodel::rpmodel( 
  tc             = 20,
  vpd            = 300,
  co2            = 400,
  fapar          = 1,
  ppfd           = seq(1, 50, by = 1),
  elv            = 0,         
  c4             = TRUE,
  do_ftemp_kphio = FALSE,
  do_soilmstress = FALSE
  ) %>% 
  as_tibble() %>% 
  mutate(setup = "ORG",
         ppfd = seq(1, 50, by = 1)) %>% 
  bind_rows(
    rpmodel::rpmodel( 
      tc             = 20,
      vpd            = 300,
      co2            = 400,
      fapar          = 1,
      ppfd           = seq(1, 50, by = 1),
      elv            = 0,         
      c4             = TRUE,
      do_ftemp_kphio = TRUE,
      do_soilmstress = FALSE
      ) %>% 
      as_tibble() %>% 
      mutate(setup = "BRC",
             ppfd = seq(1, 50, by = 1))
      )

out_ts %>% 
  ggplot(aes(x = ppfd, y = gpp, color = setup)) +
  geom_line() +
  labs(x = "PPFD (mol m-2 d-1)", y = "GPP (g m-2 d-1)")

out_ts %>% 
  ggplot(aes(x = ppfd, y = chi, color = setup)) +
  geom_line() +
  labs(x = "PPFD (mol m-2 d-1)", y = "chi")

out_ts %>% 
  ggplot(aes(x = ppfd, y = vcmax, color = setup)) +
  geom_line() +
  labs(x = "PPFD (mol m-2 d-1)", y = "Vmax (g m-2 d-1)")

## -----------------------------------------------------------------------------
calc_patm <- function( elv, patm0 = 101325 ){
  
  # Define constants:
  kTo <- 298.15    # base temperature, K (Prentice, unpublished)
  kL  <- 0.0065    # adiabiatic temperature lapse rate, K/m (Allen, 1973)
  kG  <- 9.80665   # gravitational acceleration, m/s^2 (Allen, 1973)
  kR  <- 8.3145    # universal gas constant, J/mol/K (Allen, 1973)
  kMa <- 0.028963  # molecular weight of dry air, kg/mol (Tsilingiris, 2008)
  
  # Convert elevation to pressure, Pa:
  patm <- patm0*(1.0 - kL*elv/kTo)^(kG*kMa/(kR*kL))
  
  return(patm)
}

out_ts <- rpmodel::rpmodel( 
  tc             = 20,
  vpd            = 300,
  co2            = 400,
  fapar          = 1,
  ppfd           = 30,
  patm           = calc_patm(seq(0, 4000, by = 40)),         
  c4             = TRUE,
  do_ftemp_kphio = FALSE,
  do_soilmstress = FALSE
  ) %>% 
  as_tibble() %>% 
  mutate(setup = "ORG",
         patm = calc_patm(seq(0, 4000, by = 40))) %>% 
  bind_rows(
    rpmodel::rpmodel( 
      tc             = 20,
      vpd            = 300,
      co2            = 400,
      fapar          = 1,
      ppfd           = 30,
      patm           = calc_patm(seq(0, 4000, by = 40)),         
      c4             = TRUE,
      do_ftemp_kphio = TRUE,
      do_soilmstress = FALSE
      ) %>% 
      as_tibble() %>% 
      mutate(setup = "BRC",
             patm = calc_patm(seq(0, 4000, by = 40)))
      )

out_ts %>% 
  ggplot(aes(x = patm, y = gpp, color = setup)) +
  geom_line() +
  labs(x = "P (Pa)", y = "GPP (g m-2 d-1)")

out_ts %>% 
  ggplot(aes(x = patm, y = chi, color = setup)) +
  geom_line() +
  labs(x = "P (Pa)", y = "chi")

out_ts %>% 
  ggplot(aes(x = patm, y = vcmax, color = setup)) +
  geom_line() +
  labs(x = "P (Pa)", y = "Vcmax (g m-2 d-1)")

## -----------------------------------------------------------------------------
ftemp_c3_stocker <- function(...){0.081785 * ftemp_kphio(...)}
ftemp_c3_caiprentice <- function(tc){0.044 + 0.00275 * tc - 0.425e-4 * tc^2}

ggplot() +
  geom_function(fun = ftemp_c3_stocker, args = list(c4 = FALSE)) +
  geom_function(fun = ftemp_c3_caiprentice, color = "royalblue") +
  xlim(0, 30) + 
  ylim(0, 0.1) +
  labs(x = expression(paste("Temperature (", degree, "C)")), y = expression(varphi),
       title = "Temperature dependence of quantum yield of C3 photosynthesis (BRC setup)",
       subtitle = "Black: Stocker et al., 2020 | Blue: Cai & Prentice, 2020")

ftemp_c4_stocker <- function(...){1.0 * ftemp_kphio(...)}
ftemp_c4_caiprentice <- function(tc){-0.064 + 0.03 * tc - 0.000464 * tc^2 }

ggplot() +
  geom_function(fun = ftemp_c4_stocker, args = list(c4 = TRUE), size = 2) +
  geom_function(fun = ftemp_c4_caiprentice, color = "royalblue") +
  xlim(0, 30) +
  labs(x = expression(paste("Temperature (", degree, "C)")), y = expression(varphi),
       title = "Temperature dependence of quantum yield of C4 photosynthesis (BRC setup)",
       subtitle = "Black: Stocker et al., 2020 | Blue: Cai & Prentice, 2020")

