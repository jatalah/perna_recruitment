# control script to get all useful data
library(tidyverse)

get_spat <- function(update = FALSE, active = NULL, window = NULL) {
  if (update) {
    googlesheets::gs_key('1jyxUd48fHYAJT5rEx1ySiI1fsq8E6jKRxj5vRJoYgG4', lookup = TRUE) %>%
      googlesheets::gs_read(ws = 'Full Data') %>%
      filter(!is.na(Date)) %>%
      write_rds('data/spat_sample_data.rds')
  }
  spat <- read_rds('data/spat_sample_data.rds') %>% 
    mutate(Date = as.Date(Date, '%d/%m/%Y'),
           `Specific Area` = NULL) %>% 
    filter(Date >= '1993-01-07')
  
  sites <- read_csv('data-raw/sites-lookup.csv') %>% 
    distinct(Area, Region, Lat, Long) %>% 
    drop_na
  
  if (!is.null(active)) {
    sites_active <- filter(spat, Date >= active) %>% 
      distinct(Area, Lat, Long) %>% 
      filter(Area != 'Crail Bay') %>%  # appear to have stopped sampling here?
      inner_join(select(sites, Area, Region), by = 'Area')
    sites_active <- sites %>%
      select(-Region) %>% 
      mutate_at(vars(-Area), scale, center = TRUE) %>% 
      as.data.frame %>% 
      column_to_rownames('Area') %>% 
      dist %>% 
      as.matrix %>% 
      as.data.frame %>%
      rownames_to_column('Area') %>% 
      gather(Neighbour, distance, -Area) %>% 
      filter(Neighbour %in% sites_active$Area) %>% 
      group_by(Area) %>% 
      filter(distance == min(distance)) %>% 
      mutate(Active = distance == 0, 
             distance = NULL)
    # If active site has missing obs, impute with first loc-based nearest neighbour
    spat <- spat %>% 
      left_join(sites_active, by = 'Area') %>% 
      group_by(Neighbour, Date, Lat, Long, Depth) %>% 
      mutate_at(vars(matches('Perna|Mytilus')), 
                funs(ifelse(Active & !is.na(.), ., mean(., na.rm = TRUE)))) %>% 
      ungroup %>% 
      filter(Active) %>% 
      select(-Neighbour, -Active)
  }
  spat <- left_join(spat, select(sites, Area, Region), by = 'Area')
  
  if(!is.null(window)) {
    library(lubridate)
    spat <- spat %>% 
      mutate(YearMonth = as.Date(format(Date, '%Y-%m-01'))) %>% 
      group_by(YearMonth, Region, Area, Lat, Long, Depth) %>%
      summarise(Perna = median(Perna, na.rm = TRUE)) %>%
      ungroup %>% 
      full_join(x = .,
                y = replicate(n = window, expr = ., simplify = FALSE) %>%
                  map2(1:-(window - 2),
                       ~ transmute(.x, YearMonth = YearMonth %m-% months(.y),
                                   Area, Region, Depth, Lat, Long,
                                   !!(paste0('Perna', gsub('-', '_lag', (.y - 1)))) := Perna)) %>%
                  reduce(full_join, by = c('YearMonth', 'Area', 'Region', 'Depth', 'Lat', 'Long')),
                by = c('YearMonth', 'Region', 'Area', 'Depth', 'Lat', 'Long')) %>%
      select(-Perna) %>%
      drop_na(Perna0) %>% 
      mutate(Date = YearMonth)
  }
  spat
}

get_wind <- function(update = FALSE, ym_agg = FALSE) {
  
  if (update) {
    get_curr_wind <- function(files) {
      wind_ <- wind_files %>% 
        map(~ mutate(read_csv(file.path(wind_path, .x)), File = .x) %>% 
              select(File, Station, Date.local., Dir.DegT., Speed.m.s.) %>% 
              mutate(Dir.DegT. = as.integer(Dir.DegT.))) %>% 
        reduce(bind_rows) %>% 
        separate(File, into = c('DataType', 'Wind', 'x1', 'x2', 'x3'), sep = '_') %>% 
        select(-matches('^x\\d$')) %>% 
        mutate(Station = gsub(' Aws', '', Station),
               Wind = recode(Wind, 'Daily ' = 'DailyGust', Hourly = 'HlyGust')) %>% 
        filter(Wind %in% c('HlyWind', 'DailyGust')) %>% 
        mutate(Date = Date.local.,
               # Convert degrees to radians
               Dir.Rad. = (pi * Dir.DegT.) / 180) %>% 
        group_by(Wind, Station, Date) %>% 
        summarise_at(vars(Dir.Rad., Speed.m.s.), mean, na.rm = TRUE) %>% 
        group_by(Wind) %>% 
        mutate_at(vars(Dir.Rad., Speed.m.s.), funs(as.numeric(scale(.)))) %>% 
        # remove overlapping gust data
        gather(Var, Value, Dir.Rad., Speed.m.s.) %>% 
        spread(Wind, Value)
      if ('HlyWind' %in% names(wind_)) {
        # prefer over gust
        wind_ <- wind_ %>% 
          transmute(Station, Date, Var, Value = if_else(!is.na(HlyWind), HlyWind, DailyGust))
      } else {
        wind_ <- wind_ %>% 
          transmute(Station, Date, Var, Value = DailyGust)
      }
      wind_ %>% 
        spread(Var, Value) %>%
        mutate(Region = recode(Station, 
                               'Brothers Island' = 'QCS', 
                               'Farewell Spit' = 'GB/TB', 
                               'Stephens Island' = 'PLS')) %>% 
        separate_rows(Region)
    }
    # Read all current files
    wind_path <- 'data-raw/wind'
    wind_files <- dir(wind_path, pattern = '*.csv')
    wind <- get_curr_wind()
    wind_latest <- group_by(wind, Station) %>% 
      summarise(max = max(Date)) %>% 
      pull(max) %>% 
      min
    
    library(clifro)
    cred <- cf_user('hrabel', 'taniwha91')
    stations <- cf_station(3798, 26169, 4153, 3797, 4395)
    dtypes <- list(cf_datatype(2, 1, 2, 1),
                   cf_datatype(2, 1, 3, 1),
                   cf_datatype(2, 1, 4, 1),
                   cf_datatype(2, 2, 1, 1),
                   cf_datatype(2, 2, 2, 1))
    
    get_wind_internal <- function(dtype, from) {
      while (from < Sys.Date()) {
        try({
          print(from)
          temp_query <- cf_query(user = cred,
                                 datatype = dtype,
                                 station = stations,
                                 start_date = paste0(from, '-00'))
          temp_query <- as.data.frame(temp_query@.Data, col.names = temp_query@names)
          temp_query <- mutate(temp_query,
                               `Date.local.` = strftime(strptime(`Date.local.`,
                                                                 format = '%Y%m%d:%H%M'),
                                                        '%Y-%m-%d'))
          # Update from (or break loop if stuck)
          temp_from <- max(temp_query$`Date.local.`)
        })
        if (!exists('temp_from') || temp_from == from) {
          from <- Sys.Date()
        } else {
          from <- temp_from
          # Store partial data
          write_csv(temp_query, paste0('data-raw/wind/',
                                       dtype@dt_type, '_',
                                       dtype@dt_sel_option_names[[1]], '_',
                                       min(temp_query$`Date.local.`), '_to_',
                                       from,
                                       '.csv'))
        }
      }
    }
    walk(dtypes, get_wind_internal, from = wind_latest)
    # read new files
    wind_files <- setdiff(dir(wind_path, pattern = '*.csv'), wind_files)
    if (length(wind_files) > 0) {
      wind <- get_curr_wind(wind_files) %>% 
        bind_rows(wind)
    }
    # group_by(wind, Station) %>% 
    #   summarise(max = max(Date))
    # Surface drag coef * air density * euclid dist of east & north wind decomps
    wind %>% 
      mutate(SurfDrag = (0.75 + 0.067 * Speed.m.s.) * 0.01,
             EastComp = -Speed.m.s. * sin(Dir.Rad.),
             NorthComp = -Speed.m.s. * cos(Dir.Rad. - (30 * pi / 180)),
             TauCS = SurfDrag * 1.3 *
               sqrt((EastComp ^ 2) + (NorthComp ^ 2)) * EastComp,
             TauAS = SurfDrag * 1.3 *
               sqrt((EastComp ^ 2) + (NorthComp ^ 2)) * NorthComp) %>% 
      select(-SurfDrag, -EastComp, -NorthComp, -Speed.m.s., -Dir.Rad.) %>% 
      write_csv('data-raw/wind-vectors.csv')
  }
  if (ym_agg) {
    read_csv('data-raw/wind-vectors.csv') %>% 
      select(-Station) %>% 
      mutate(YearMonth = as.Date(format(Date, '%Y-%m-01')), Date = NULL) %>% 
      group_by(Region, YearMonth) %>% 
      summarise_all(mean, na.rm = TRUE)
    # anomalies here?
  } else {
    read_csv('data-raw/wind-vectors.csv') %>% 
      select(-Station)
  }
  
}

get_tides <- function(update = FALSE, ym_agg = FALSE) {
  tides <- dir('data-raw/tides/', '*.csv', full.names = TRUE) %>%
    map(~ read_csv(.x, 
                   skip = 3, 
                   col_names = c('Day', 'WeekDay', 'Month', 'Year',
                                 paste0(c('Time_', 'Height_'), rep(1:4, 2)[order(rep(1:4, 2))])),
                   col_types = c('iciicdcdcdcd')) %>%
          mutate(Location = gsub('\\d*|_.*$', '', strsplit(.x, '/')[[1]][3]))) %>%
    reduce(bind_rows) %>%
    gather(Key, Value, matches('Height|Time')) %>%
    separate(Key, c('Key', 'Tide'), sep = '_') %>%
    spread(Key, Value) %>%
    transmute(Date = as.Date(paste(Year, Month, Day, sep = '-'), '%Y-%m-%d'),
              Time,
              Region = gsub('\\s\\.csv', '', Location),
              Region = recode(Region, 
                              'Havelock' = 'PLS', 
                              'Kaiteriteri' = 'TB',
                              'Picton' = 'QCS', 
                              'Tarakohe' = 'GB'),
              Tide = as.integer(Tide),
              Height = as.double(Height)) %>%
    group_by(Date, Region) %>% 
    summarise(Min_Tide = min(Height, na.rm = TRUE),
              Max_Tide = max(Height, na.rm = TRUE)) %>% 
    ungroup()
  
  if (update) {
    group_by(tides, Region) %>% 
      summarise(Date = max(Date)) %>% 
      mutate(Year = format(Date, '%Y')) %>% 
      pull(Year) %>% 
      min()
    # to do
    # http://www.linz.govt.nz/docs/hydro/tidal-info/tide-tables/maj-ports/csv/Havelock%202018.csv
    # http://www.linz.govt.nz/docs/hydro/tidal-info/tide-tables/maj-ports/csv/Kaiteriteri%202018.csv
    # http://www.linz.govt.nz/docs/hydro/tidal-info/tide-tables/maj-ports/csv/Picton%202018.csv
    # http://www.linz.govt.nz/docs/hydro/tidal-info/tide-tables/maj-ports/csv/Tarakohe%202018.csv
  }
  if (ym_agg) {
    tides %>% 
      mutate(YearMonth = as.Date(format(Date, '%Y-%m-01')), Date = NULL) %>% 
      group_by(Region, YearMonth) %>% 
      summarise(Min_Tide = min(Min_Tide, na.rm = TRUE), 
                Max_Tide = max(Max_Tide, na.rm = TRUE))
    # anomalies here?
  } else {
    tides
  }
}

get_soi <- function(update = FALSE) {
  if (update) {
    readr::read_table('https://crudata.uea.ac.uk/cru/data/soi/soi_3dp.dat',
                      col_names = c('Year', 1:12),
                      col_types = paste0('i', paste0(rep('d', 12), collapse = ''))) %>%
      tidyr::gather(Month, SOI, -Year) %>% 
      transmute(Date = as.Date(paste0(Year, '-', Month, '-01')), 
                SOI = if_else(SOI < -99, NA_real_, as.double(SOI))) %>% 
      readr::write_csv('data-raw/soi.csv')
    # readr::read_csv('https://www.ncdc.noaa.gov/teleconnections/enso/indicators/soi/data.csv',
    #                 skip = 1, col_types = 'cd') %>%
    #   transmute(Date = as.Date(paste0(Date, '01'), format = '%Y%m%d'),
    #             SOI = Value) %>%
    #   write_csv('data-raw/soi.csv')
  }
  read_csv('data-raw/soi.csv')
}

get_cliflo <- function(update = FALSE, ym_agg = FALSE) {
  
  if (update) {
    get_curr_cliflo <- function(files) {
      map(files, read_csv, col_types = 'cDidididddddddddddddddd') %>%
        bind_rows %>%
        distinct %>%
        set_names(sub('\\(.*\\)$|\\..*$', '', names(.)))
    }
    
    cliflo_files <- dir('data-raw/cliflo/', full.names = TRUE)
    cliflo_data <- lapply(cliflo_files, read_csv, col_types = 'cDidididddddddddddddddd') %>%
      lapply(function(x) set_names(x, sub('\\(.*\\)$|\\..*$', '', names(x)))) %>% 
      bind_rows
    
    # range(cliflo_data$Day, na.rm = TRUE)
    
    sites <- read_csv('data-raw/sites-lookup.csv') %>% 
      distinct(Area, Lat, Long) %>% 
      drop_na
    
    stations_raw <- read_csv('data-raw/stations-cliflo.csv')
    stations <- stations_raw %>% 
      distinct(Area, Lat, Long) %>% 
      drop_na
    
    site_dist <- bind_rows(sites, stations) %>% 
      mutate_at(vars(-Area), scale, center = TRUE) %>% 
      as.data.frame %>% 
      column_to_rownames('Area') %>% 
      dist %>% 
      as.matrix %>% 
      as.data.frame %>% 
      rownames_to_column('Station') %>% 
      gather(Neighbour, distance, -Station) %>% 
      filter(Neighbour %in% sites$Area) %>% 
      filter(Station %in% stations$Area) %>% 
      transmute(Station, Neighbour, Dist = 1 / distance ^ 2)
    
    library(clifro)
    cred <- cf_user('hrabel', 'taniwha91')
    stations <- cf_station(stations_raw$Agent)
    dtypes <- cf_datatype(1, 1, 2, 1)
    
    from <- max(cliflo_data$Day, na.rm = TRUE)
    while (from < Sys.Date()) {
      try({
        print(from)
        temp_query <- cf_query(user = cred,
                               datatype = dtypes,
                               station = stations,
                               start_date = paste0(from, '-00'))
        temp_query <- as.data.frame(temp_query@.Data, col.names = temp_query@names)
        temp_query <- mutate(temp_query,
                             Day.Local_Date. = strftime(strptime(Day.Local_Date.,
                                                                 format = '%Y%m%d:%H%M'),
                                                        '%Y-%m-%d')) %>% 
          rename(Day = Day.Local_Date.)
        # Update from (or break loop if stuck)
        temp_from <- max(temp_query$Day)
      })
      if (!exists('temp_from') || temp_from == from) {
        from <- Sys.Date()
      } else {
        from <- temp_from
        # Store partial data
        write_csv(temp_query, paste0('data-raw/cliflo/data_',
                                     min(temp_query$Day), '_to_',
                                     from,
                                     '.csv'))
      }
    }
    
    cliflo_files <- setdiff(dir('data-raw/cliflo/', full.names = TRUE), cliflo_files)
    
    cliflo_data <- get_curr_cliflo(cliflo_files) %>% 
      bind_rows(cliflo_data)
    
    # range(cliflo_data$Day, na.rm = TRUE)
    
    cliflo_data[, c('Station', 'Day', 'Rain', 'Tdry', 'TWet', 'RH', 'Tmax', 'Tmin', 'Tgmin',
                    'ET05', 'ET10', 'ET20', 'ET30', 'ET100', 'Pmsl', 'Pstn', 'Sun', 'Rad')] %>%
      ungroup() %>% 
      rename(Date = Day) %>% 
      inner_join(site_dist, by = 'Station') %>%
      filter(Neighbour %in% unique(sites$Area)) %>%
      group_by(Neighbour, Date) %>%
      summarise_at(vars(-Station, -Dist, -Date, -Neighbour), 
                   funs(weighted.mean(., w = Dist, na.rm = TRUE))) %>%
      readr::write_csv('data-raw/interpolated_cliflo_data.csv')
  }
  if (ym_agg) {
    read_csv('data-raw/interpolated_cliflo_data.csv') %>% 
      mutate(YearMonth = as.Date(format(Date, '%Y-%m-01')), Date = NULL) %>% 
      group_by(Neighbour, YearMonth) %>% 
      summarise_all(mean, na.rm = TRUE) %>% 
      ungroup %>% 
      select(-Pstn, -ET05)
    # anomalies here?
  } else {
    read_csv('data-raw/interpolated_cliflo_data.csv') %>% 
      select(-Pstn, -ET05)
  }
}

fetch <- read_csv('data-raw/sites-fetch.csv') %>% 
  select(-Lat, -Long)

bathy <- read_csv('data-raw/sites-bathy.csv') %>% 
  select(-Lat, -Long)

spat_w_all <- function(update = FALSE, window = NULL, response = 'Perna') {
  # drops <- switch(response, 'Perna' = 'Mytilus', 'Mytilus' = 'Perna', 'Both' = '')
  if (!is.null(window)) {
    get_spat(update, window = window) %>%
      mutate(YearMonth = as.Date(format(Date, '%Y-%m-01')), Date = NULL) %>% 
      full_join(bathy, by = 'Area') %>% 
      full_join(fetch, by = 'Area') %>% 
      full_join(get_soi(update), by = c('YearMonth' = 'Date')) %>% 
      full_join(get_cliflo(update, ym_agg = TRUE), by = c('YearMonth', 'Area' = 'Neighbour')) %>%
      full_join(get_wind(update, ym_agg = TRUE), by = c('YearMonth', 'Region')) %>% 
      full_join(get_tides(update, ym_agg = TRUE), by = c('YearMonth', 'Region'))
  } else {
    cliflo <- get_cliflo(update)
    wind <- get_wind(update)
    tides <- get_tides(update)
    get_spat(update) %>%
      full_join(bathy, by = 'Area') %>% 
      select(-Bathymetry) %>% 
      full_join(cliflo, by = c('Date', 'Area' = 'Neighbour')) %>%
      group_by(Area) %>% 
      mutate(Region = first(Region[!is.na(Region)])) %>% 
      full_join(wind, by = c('Date', 'Region')) %>% 
      full_join(tides, by = c('Date', 'Region')) %>% 
      mutate(YearMonth = as.Date(format(Date, '%Y-%m-01'))) %>% 
      full_join(get_soi(update), by = c('YearMonth' = 'Date')) %>% 
      full_join(fetch, by = 'Area') %>%
      full_join(bathy, by = 'Area')
  }
}

og_dir <- setwd("H:/Common/Biosecurity Team/Projects/CuSP/Perna spat modelling - CuSP 15345/Data analyses")
# Note Perna0 is next month median as response
setwd(og_dir)

spat <- 
  spat_w_all(update = FALSE) %>%
  write_rds(file.path(og_dir, 'data/spat.RDS'))


# END #############################################################################################################


spat %>%
# .Last.value %>% 
  filter(Date >= '1992-12-07' & Date <= '2018-10-08') %>% 
  filter(!is.na(Date) & !is.na(Perna)) %>% 
  arrange(desc(Date)) %>% 
  View()


.Last.value -> spat
spat %>% 
  filter(Date >= '1992-12-07' & Date <= '2018-10-08') %>% 
  write_rds(file.path(og_dir, 'data/spat.RDS'))

spat %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, missing) %>% 
  arrange(desc(missing))
spat %>% 
  drop_na(-Mytilus) %>% 
  summarise_at('Date', funs(min, max))

spat_all <- spat_w_all(window = 1) %>%
  write_rds('data/perna_window1.RDS')
# 
# na_summary <- function(df) {
#   df %>% 
#     summarise_all(funs(n = n(), nas = sum(is.na(.)))) %>% 
#     gather(var, val) %>% 
#     separate(var, c('var', 'fun'), '_(?=[^_]*$)') %>% 
#     spread(fun, val) %>% 
#     arrange(desc(nas)) %>% 
#     print(n = nrow(.))
# }
# 
# spat_agg %>% 
#   na_summary
# 
# spat_all %>% 
#   na_summary
# 
# spat_agg %>% 
#   drop_na
mfa_pls <- readxl::read_excel('data-raw/Spat results Pelorus PU & EB 1974 onwards.xls',
                              guess_max = 99999999) %>% 
  select(Site = Bay, SiteNo = `site number`) %>% 
  distinct %>% 
  group_by(Site) %>% 
  mutate(SiteNo = if_else(is.na(SiteNo), mean(SiteNo, na.rm = TRUE), SiteNo)) %>% 
  ungroup

bmop_crds <- readr::read_csv('data-raw/sites-lookup.csv') %>% 
  select(Area, Specific_Area, Lat, Long) %>% 
  distinct()



mfa_pls
spat_all <- spat_w_all(response = 'Both') %>% 
  left_join(inner_join(mfa_pls, bmop_crds, by = c('Site' = 'Specific_Area')) %>% 
              distinct(Area, SiteNo), 
            by = c('Area')) %>% 
  write_csv('for_barrie/spat.csv')

spat <- .Last.value$value
spat
