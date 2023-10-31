#-------------------------------------------------------------------------------
# Average precipitation for a radius around central coordinates
# Inputs: Coordinates, a radius, and dates
#   
# Outputs: A CSV of precipitation data
#   
#-------------------------------------------------------------------------------

library(ncdf4)
library(s2)


get_nc_file <- function(file_number) {
  filename = paste0(file_number, ".nc")
  nc_file = nc_open(filename)
  global_precip = ncvar_get(nc_file)
  nc_close(nc_file)
  return(global_precip)
}

average_precipitation <- function(lat, lon, global_precip, radius_m=50000, 
                                  coord_delta=1) {
  global_precip[lat*10+90, lon*10+180]
  center = s2_lnglat(lat, lon)
  n = coord_delta*2/0.1
  lats = matrix(rep(seq(lat-coord_delta, lat+coord_delta, 0.1), n+1), nrow=n+1)
  lons = t(matrix(rep(seq(lon-coord_delta, lon+coord_delta, 0.1), n+1), nrow=n+1))
  candidate_points = s2_lnglat(lats, lons)
  within_radius = s2_distance(center, candidate_points)<radius_m
  points_within_radius = candidate_points[within_radius]
  total_precip = 0
  for (coord in points_within_radius) {
    total_precip = total_precip + global_precip[coord[1]*10+90,coord[2]*10+180]
  }
  avg_precip = total_precip/length(points_within_radius)
  return(avg_precip)
}

average_precipitation_over_time <- function(coords, start_file_number,
                                            end_file_number, 
                                            time_delta="daily") {
  if (time_delta=="daily") {
    file_seq = NULL
    while (end_file_number-start_file_number > 400) {
      year_num = start_file_number - (start_file_number %% 1000)
      temp_end_file_number = year_num+365
      file_seq = c(file_seq, start_file_number:temp_end_file_number)
      start_file_number = year_num+1001
    }
    file_seq = c(file_seq, start_file_number:end_file_number)
  } else {
    errorCondition('Not Implemented')
  }
  nrows = length(file_seq)*ncol(coords)
  average_precipitation_df = data.frame(datetime=character(nrows),
    rainfall=numeric(nrows), lat=numeric(nrows), lon=numeric(nrows))
  for (file_number_i in 1:length(file_seq)) {
    global_precip = get_nc_file(file_seq[file_number_i])
    for (coord_i in 1:ncol(coords)) {
      day = file_seq[file_number_i] %% 1000
      year = file_seq[file_number_i] %/% 1000
      average_precipitation_df$datetime[file_number_i*coord_i] = 
        as.character(as.Date(day, origin = paste0(year-1, "-12-31"), 
                             format="%Y-%m-%d"))
      average_precipitation_df$rainfall[file_number_i*coord_i] = 
        average_precipitation(coords[1][coord_i], coords[2][coord_i], 
                              global_precip)
      average_precipitation_df$lat[file_number_i*coord_i] = coords[1][coord_i]
      average_precipitation_df$lon[file_number_i*coord_i] = coords[2][coord_i] 
    }
  }
  return(average_precipitation_df)
}

# VITAL (Rehri Goth, Pakistan)
# 2018-09-10 (2018253) to 2020-06-24 (2020176)
rehri_goth_precipitation = average_precipitation_over_time(cbind(c(24.8, 67.2)),
                                                           2018153, 2020276)
rehri_goth_precipitation$study = c(rep("VITAL", nrow(rehri_goth_precipitation)))

# ELICIT (Haydom, Tanzania)
# 2017-09-15 (2017258) to 2019-10-23 (2019296)
haydom_precipitation = average_precipitation_over_time(cbind(c(-4.2, 35.0)), 
                                                       2017158, 2020031)
haydom_precipitation$study = c(rep("ELICIT", nrow(haydom_precipitation)))

# MISAME (Hounde District, Burkina Faso)
# 2020-08-23 (2020236) to 2022-03-01 (2022060)
hounde_precipitation = average_precipitation_over_time(cbind(c(11.5, -3.5)), 
                                                       2020136, 2022160)
hounde_precipitation$study = c(rep("MISAME", nrow(hounde_precipitation)))

all_precipitation = rbind(rehri_goth_precipitation, haydom_precipitation, hounde_precipitation)
write.csv(all_precipitation, paste0(here(),"/data/all_precipitation.csv"), row.names=FALSE)
