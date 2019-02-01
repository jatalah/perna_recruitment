pred_data <- readr::read_csv("spat_lead_vars_1993_onwards.csv") %>%
  select(Date, Depth, Perna, Mytilus, Lat, Long, WDir, WSpd, GustDir,
         GustSpd, WindRun, Rain, ET100, Rad, Sun, Bathymetry, SOI) %>%
  # Outliers
  filter(Perna < 600000, Mytilus < 80000) %>%
  mutate(Date = zoo::as.yearmon(Date, "%Y-%m"),
         Year = as.integer(format(Date, "%Y")),
         Month = as.integer(format(Date, "%m")),
         Lat = as.double(Lat),
         Long = as.double(Long)) %>%
  # Surface drag coef * air density * euclid dist of east & north wind decomps
  mutate(WSurfDrag = (0.75 + 0.067 * WSpd) * 0.01,
         GSurfDrag = (0.75 + 0.067 * GustSpd) * 0.01,
         WDirRad = WDir * pi / 180,
         GDirRad = GustDir * pi / 180,
         WEastComp = -WSpd * sin(WDirRad),
         WNorthComp = -WSpd * cos(WDirRad - (30 * pi / 180)),
         WTauCS = WSurfDrag * 1.3 * sqrt((WEastComp ^ 2) + (WNorthComp ^ 2)) * WEastComp,
         WTauAS = WSurfDrag * 1.3 * sqrt((WEastComp ^ 2) + (WNorthComp ^ 2)) * WNorthComp,
         GEastComp = -GustSpd * sin(GDirRad),
         GNorthComp = -GustSpd * cos(GDirRad - (30 * pi / 180)),
         GTauCS = GSurfDrag * 1.3 * sqrt((GEastComp ^ 2) + (GNorthComp ^ 2)) * GEastComp,
         GTauAS = GSurfDrag * 1.3 * sqrt((GEastComp ^ 2) + (GNorthComp ^ 2)) * GNorthComp)
