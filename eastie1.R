water_folder <- "/Users/kevinledezma/Downloads/eastie_farm_corr/WaterFurnaceSymphonyData"
paths_water <- list.files(water_folder, pattern = c(".csv",".xlsx"), full.names = TRUE, recursive = TRUE)
water_dat<-paths_water |> map(lessR::Read) |> 
  list_rbind()
water_dat<-water_dat |>
  mutate(time_stamp = coalesce(water_dat$X, water_dat$logtime))|>
  select(-X, -logtime) 
water_tem_dat <- water_dat %>%
  group_by(time_stamp) %>%
  summarize(
    heating_avg = mean(Heating..Part...kWh. + Heating..Full...kWh.),
    cooling_avg = mean(Cooling..Part...kWh. + Cooling..Full...kWh.)
  ) %>%
  ungroup()
water_tem_dat <- water_tem_dat %>%
  separate(time_stamp, into = c("Date", "Time"), sep = " ")
logan_airport<-read.csv("/Users/kevinledezma/Downloads/eastie_farm_corr/OutdoorTemperatures/KBOSdailydata_Jan23-Sept23.csv")
as_tibble(logan_airport)
colnames(logan_airport)<-tolower(colnames(logan_airport))
colnames(water_tem_dat)<-tolower(colnames(water_tem_dat))
merge_water_logan<-merge(water_tem_dat, logan_airport, by="date")
merge_water_logan <- select(merge_water_logan, -wt01,-wt02,-wt03,-wt04,-wt06,-wt08,-wt09)
cooling<-merge_water_logan %>%
  filter(tavg >=60)
heating<-merge_water_logan %>%
  filter(tavg <= 67)
cooling_model <- glm(cooling_avg~tavg, data = cooling, family = gaussian)
heating_model <- glm(heating_avg~tavg, data = heating, family = gaussian)