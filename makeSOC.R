#' Make soil carbon stocks
#' 
#' Calculate the soil carbon stocks at steady state (ie a stable/mature ecosystem) given parameters.
#' Based on a one pool model $\frac{dC}{dt} = u_{npp} - k Q_{10}^{(T-T_0)/10} C$
#' where $C$ is the soil carbon stock $u_{npp}$ the input into the soil approximated by
#' net primary production, $k$ the decomposition rate, $Q_{10}$ the factor of increase
#' in decay rate given 10$^\circ$ C of warming, $T$ the mean annual temperature,
#' $T_0$ the reference temperature.
#' Optionally include temperature.
#' This function is designed to be used with `raster::calc` or `raster::overlay`.
#' 
#' We expect SOC to range between 0-40 kg m^-2; and NPP to range between 0-2 km m^-2 yr-1.
#' Default parameter ranges (min, median, max) are turnover time in years (10, 15, 38) and  Q10: (1.45, 1.7, 2.7) from 
#' Todd-Brown, K. E. O., Randerson, J. T., Post, W. M., Hoffman, F. M., Tarnocai, C., Schuur, E. A. G., and Allison, S. D.: Causes of variation in soil carbon simulations from CMIP5 Earth system models and comparison with observations, Biogeosciences, 10, 1717–1736, https://doi.org/10.5194/bg-10-1717-2013, 2013
#' 
#' Field-warmed Q10 values 2.2 [1.6, 2.7] 95\% CI from 
#' Todd-Brown, K., Zheng, B., and Crowther, T. W.: Field-warmed soil carbon changes imply high 21st-century modeling uncertainty, Biogeosciences, 15, 3659–3671, https://doi.org/10.5194/bg-15-3659-2018, 2018.
#' 
#' Moisture function is piece wise linear with specified min/max moisture modifiers and some optimal moisture condition. [[TODO fill in references here]]
#' 
#' @param npp a numerical value of net primary production that is an area density per year
#' @param turnover_time the turnover time in years
#' @param temperature a numerical value of temperature in degrees C
#' @param Q10 the factor increase in temperature sensitivity
#' @param T0 the reference temperater for the temperature sensitivity function.
#' @param moisture a numerical value of the fraction of filled pore water
#' @param lower_moisture_modifer a numerical value of the fraction of decay rate (compared to optimal) at 0 fraction of pore filled water
#' @param upper_moisture_modifier a numerical value of the fraction of decay rate (compared to optimal) at 1 fraction of pore filled water
#' @param optimize_moisture a numerical value of the fraction of optimal (max decay rate) pore water
#'
#' @return the soil carbon stocks under steady state assumption
#' @export
#'
#' @examples
#' makeSOC(npp = 1)
makeSOC <- function(npp, 
                    turnover_time = 15, #CMIP5: min, median, max
                    #Temperature modifications
                    temperature = NULL, 
                    Q10 = 2, #CMIP5: min, median, max
                    T0 = 15, #hard code set
                    #moisture parameters are current guesses
                    moisture = NULL, #moisture filled pore space
                    lower_moisture_modifer = 0.2,# fraction of optimal decay at 0
                    upper_moisture_modifier = 0.9,# fraction of optimal decay at 1
                    optimize_moisture = 0.6){ #optimal decay fraction
  #$dC/dt = u - kC
  
  #Table 4 - Todd-Brown etal 2013 (https://bg.copernicus.org/articles/10/1717/2013/bg-10-1717-2013.html)
  # CMIP5_analysis <- read.csv(text = 
  # 'model, R2_k, R2_kT, R2_kTM, turnover, Q10,
  # CCSM4, 0.65, 0.88, 0.88, 11.5, 1.55,
  # NorESM1, 0.61, 0.88, 0.88, 14.9, 1.65,
  # BCC-CSM1.1, NA, 0.89, 0.89, 16.8, 2.05,
  # HadGEM2, 0.27, 0.79, 0.85, 13.5, 1.52,
  # IPSL-CM5, NA, 0.93, 0.93, 13.2, 1.61,
  # GFDL-ESM2G, NA, 0.85, 0.89, 10.9, 2.61,
  # CanESM2, NA, 0.56, 0.73, 22.5, 1.74,
  # INM-CM4, NA, 0.72, 0.72, 20.7, 2.19,
  # GISS-E2, NA, NA, NA, NA, NA,
  # MIROC-ESM, NA, 0.62, 0.62, 37.1, 1.98,
  # MPI-ESM-LR, NA, 0.32, 0.32, 29.8, 1.45')
  modifier <- 1
  
  if(!is.null(temperature)){
    modifier <- modifer * Q10 ^(-(temperature-T0)/10)
  }
  
  if(!is.null(moisture)){
    
    if(any(moisture > 1 | moisture < 0)){
      stop('moisture must be between 0 and 1')
    }
    # lower_moisture_modifer <- 0.2
    # upper_moisture_modifier <- 0.9
    # optimize_moisture <- 0.8
    # 
    # moisture <- 0.9
    mod <- (1-lower_moisture_modifer) / (optimize_moisture - 0) * moisture + lower_moisture_modifer
    
    slope <- (upper_moisture_modifier - 1) / (1 - optimize_moisture)
    upper_mod <- slope * moisture + slope * (0 - optimize_moisture) + 1
    
    mod[moisture > optimize_moisture] <- upper_mod[moisture > optimize_moisture]
    
    modifier <- modifer * mod
  }
  
  return(npp * turnover_time * modifer)
}