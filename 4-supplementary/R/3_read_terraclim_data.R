
library(ncdf4)
library(geosphere)
library(here)
source(here::here("2-initial-analysis/R", "0-config.R"))


# aet: actual evapotranspiration
# ppt: precipitation
# soil: soil moisture
# tmax: max temperature
# tmin: min temperature


# WASHB gps data
df_gps <- read_dta(file = here::here("1-data", "0-untouched",
                                     "6. WASHB_Baseline_gps.dta"))
#df_publicid <- read_csv(file = here::here("1-data", "0-untouched",
                                          #"public-ids.csv"))

ll_diar = df_gps %>% dplyr::select(qgpslong, qgpslat)


### Functions from https://gist.github.com/jadebc/e38c5fbea9006fa7fb5911ca741223dd
process_terraclimate = function(var, coords, year){
  filename <- paste0(here::here(), "/1-data/0-terraclim/", var, "/TerraClimate_", var, "_",
                     year, ".nc")
  
  x = as.numeric(coords)
  
  nc <- nc_open(filename)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  flat = match(abs(lat - x[2]) < 1/48, 1)
  latindex = which(flat %in% 1)
  lat_match = lat[latindex]
  
  flon = match(abs(lon - x[1]) < 1/48, 1)
  lonindex = which(flon %in% 1)
  long_match = lon[lonindex]
  start <- c(lonindex, latindex, 1)
  count <- c(1, 1, -1)
  
  distance = distm(c(long_match, lat_match), x, fun = distHaversine)
  
  # read in the full period of record using aggregated files
  data <- as.numeric(ncvar_get(nc, varid = var,start = start, count))
  data = data %>% append(distance)
  return(data)
}

compile_terraclimate = function(latlong, var, year){
  #latlong = ll # temp, to speed up computations for testing 
  print(year)
  raw = apply(latlong,1, function(x) process_terraclimate(
    var = var, coords = x, year = year))
  raw_df = as.data.frame(raw)
  
  generate_terraclimate_table = function(month){
    tbl = as.data.frame(cbind(latlong[1], latlong[2], month, year, t(raw_df[month, ]), t(raw_df[13, ])))
    colnames(tbl) = c("qgpslong", "qgpslat", "month", "year", var, "distance_from_source")
    return(tbl)
  }
  
  df = lapply(1:12, generate_terraclimate_table) %>% bind_rows()
  
  return (df)
}


aet = lapply(c(2012, 2013, 2014, 2015), 
             function(x) compile_terraclimate(latlong = ll_diar, var = "aet", year = x)) %>% 
  bind_rows()
saveRDS(aet, paste0(here::here(),"/1-data/2-final/washb-bgd-aet.RDS"))

ppt = lapply(c(2012, 2013, 2014, 2015), 
             function(x) compile_terraclimate(latlong = ll_diar, var = "ppt", year = x)) %>% 
  bind_rows()
saveRDS(ppt, paste0(here::here(),"/1-data/2-final/washb-bgd-ppt.RDS"))

soil = lapply(c(2012, 2013, 2014, 2015), 
             function(x) compile_terraclimate(latlong = ll_diar, var = "soil", year = x)) %>% 
  bind_rows()
saveRDS(soil, paste0(here::here(),"/1-data/2-final/washb-bgd-soil.RDS"))

soil = lapply(c(2012, 2013, 2014, 2015), 
              function(x) compile_terraclimate(latlong = ll_diar, var = "soil", year = x)) %>% 
  bind_rows()
saveRDS(soil, paste0(here::here(),"/1-data/2-final/washb-bgd-soil.RDS"))

tmax = lapply(c(2012, 2013, 2014, 2015), 
              function(x) compile_terraclimate(latlong = ll_diar, var = "tmax", year = x)) %>% 
  bind_rows()
saveRDS(tmax, paste0(here::here(),"/1-data/2-final/washb-bgd-tmax.RDS"))

tmin = lapply(c(2012, 2013, 2014, 2015), 
              function(x) compile_terraclimate(latlong = ll_diar, var = "tmin", year = x)) %>% 
  bind_rows()
saveRDS(tmin, paste0(here::here(),"/1-data/2-final/washb-bgd-tmin.RDS"))
