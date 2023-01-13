library(sfdep)
library(readr)
library(sf)
library(ggplot2)
library(rgdal)
library(rgeos)


Shapefile <- choose.files()

Data_2017 <- read_csv(choose.files(), col_types = "ccidD")
Data_2018 <- read_csv(choose.files(), col_types = "ccidD")
Data_2019 <- read_csv(choose.files(), col_types = "ccidD")
Data_2020 <- read_csv(choose.files(), col_types = "ccidD")
Data_2021 <- read_csv(choose.files(), col_types = "ccidD")


Wards <- read_sf(Shapefile)

Geog <- readOGR(Shapefile)
names(Geog)

Geog_ft <- fortify(Geog, region = "ward")

Data_df_2017 <- subset(Data_2017, select = -c(Population, Lat, Long, Name, Add, Month_1, pop2009))
Data_df_2018 <- subset(Data_2018, select = -c(Population, Lat, Long, Name, Add, Month_1, pop2009))
Data_df_2019 <- subset(Data_2019, select = -c(Population, Lat, Long, Name, Add, Month_1, pop2009))
Data_df_2020 <- subset(Data_2020, select = -c(Population, Lat, Long, Name, Add, Month_1, pop2009))
Data_df_2021 <- subset(Data_2021, select = -c(Population, Lat, Long, Name, Add, Month_1, pop2009))
colnames(Data_df_2017)[1] <- "ward"
colnames(Data_df_2018)[1] <- "ward"
colnames(Data_df_2019)[1] <- "ward"
colnames(Data_df_2020)[1] <- "ward"
colnames(Data_df_2021)[1] <- "ward"

# Formating the date using as.Date function. 
Data_df_2017$Date <- as.Date(Data_df_2017$Date, format = "%d/%m/%Y")
Data_df_2018$Date <- as.Date(Data_df_2018$Date, format = "%d/%m/%Y")
Data_df_2019$Date <- as.Date(Data_df_2019$Date, format = "%d/%m/%Y")
Data_df_2020$Date <- as.Date(Data_df_2020$Date, format = "%d/%m/%Y")
Data_df_2021$Date <- as.Date(Data_df_2021$Date, format = "%d/%m/%Y")

Geo_df <- subset(Wards, select = -c(gid, county, subcounty, Longitude, Latitude, pop2009))
Geo <- st_make_valid(Geo_df)
st_is_valid(Geo)

bos_stc_2017 <- spacetime(Data_df_2017, Geo,
                     .loc_col = "ward",
                     .time_col = "Date")

is_spacetime_cube(bos_stc_2017)

bos_stc_2018 <- spacetime(Data_df_2018, Geo,
                          .loc_col = "ward",
                          .time_col = "Date")

is_spacetime_cube(bos_stc_2018)

bos_stc_2019 <- spacetime(Data_df_2019, Geo,
                          .loc_col = "ward",
                          .time_col = "Date")

is_spacetime_cube(bos_stc_2019)

bos_stc_2020 <- spacetime(Data_df_2020, Geo,
                          .loc_col = "ward",
                          .time_col = "Date")

is_spacetime_cube(bos_stc_2020)

bos_stc_2021 <- spacetime(Data_df_2021, Geo,
                          .loc_col = "ward",
                          .time_col = "Date")

is_spacetime_cube(bos_stc_2021)

library(dplyr)

bos_nb_2017 <- bos_stc_2017 |>
  activate("geometry") |>
  mutate(
    nb = include_self(st_contiguity(geometry)),
    wt = st_weights(nb)
  ) |>
  set_nbs("nb") |>
  set_wts("wt")

head(bos_nb_2017)

bos_nb_2018 <- bos_stc_2018 |>
  activate("geometry") |>
  mutate(
    nb = include_self(st_contiguity(geometry)),
    wt = st_weights(nb)
  ) |>
  set_nbs("nb") |>
  set_wts("wt")

head(bos_nb_2018)

bos_nb_2019 <- bos_stc_2019 |>
  activate("geometry") |>
  mutate(
    nb = include_self(st_contiguity(geometry)),
    wt = st_weights(nb)
  ) |>
  set_nbs("nb") |>
  set_wts("wt")

head(bos_nb_2019)

bos_nb_2020 <- bos_stc_2020 |>
  activate("geometry") |>
  mutate(
    nb = include_self(st_contiguity(geometry)),
    wt = st_weights(nb)
  ) |>
  set_nbs("nb") |>
  set_wts("wt")

head(bos_nb_2020)

bos_nb_2021 <- bos_stc_2021 |>
  activate("geometry") |>
  mutate(
    nb = include_self(st_contiguity(geometry)),
    wt = st_weights(nb)
  ) |>
  set_nbs("nb") |>
  set_wts("wt")

head(bos_nb_2021)


gi_stars_2017 <- bos_nb_2017 |>
  group_by(Date) |> 
  mutate(gi_star = local_gstar_perm(Cases, nb, wt)) |>
  tidyr::unnest(gi_star)

gi_stars_2018 <- bos_nb_2018 |>
  group_by(Date) |> 
  mutate(gi_star = local_gstar_perm(Cases, nb, wt)) |>
  tidyr::unnest(gi_star)

gi_stars_2019 <- bos_nb_2019 |>
  group_by(Date) |> 
  mutate(gi_star = local_gstar_perm(Cases, nb, wt)) |>
  tidyr::unnest(gi_star)

gi_stars_2020 <- bos_nb_2020 |>
  group_by(Date) |> 
  mutate(gi_star = local_gstar_perm(Cases, nb, wt)) |>
  tidyr::unnest(gi_star)

gi_stars_2021 <- bos_nb_2021 |>
  group_by(Date) |> 
  mutate(gi_star = local_gstar_perm(Cases, nb, wt)) |>
  tidyr::unnest(gi_star)


cbg_2017 <- gi_stars_2017 |> 
  ungroup() |>
  filter(ID =="131") |>
  select(ward, Date, gi_star)

cbg_2017 |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)

cbg_2018 <- gi_stars_2018 |> 
  ungroup() |>
  filter(ID =="131") |>
  select(ward, Date, gi_star)

cbg_2018 |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)

cbg_2019 <- gi_stars_2019 |> 
  ungroup() |>
  filter(ID =="131") |>
  select(ward, Date, gi_star)

cbg_2019 |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)

cbg_2020 <- gi_stars_2020 |> 
  ungroup() |>
  filter(ID =="131") |>
  select(ward, Date, gi_star)

cbg_2020 |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)

cbg_2021 <- gi_stars_2021 |> 
  ungroup() |>
  filter(ID =="131") |>
  select(ward, Date, gi_star)

cbg_2021 |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)

ehsa_manual_2017 <- gi_stars_2017|>
  group_by(ward, ID) |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)

ehsa_manual_2018 <- gi_stars_2018|>
  group_by(ward, ID) |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)

ehsa_manual_2019 <- gi_stars_2019|>
  group_by(ward, ID) |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)

ehsa_manual_2020 <- gi_stars_2020|>
  group_by(ward, ID) |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)

ehsa_manual_2021 <- gi_stars_2021|>
  group_by(ward, ID) |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  tidyr::unnest_wider(mk)


colnames(ehsa_manual_2017)[1] <- "ward"
ehsa_2017 <- merge(Geo, ehsa_manual_2017, by = "ID", all.x = TRUE)
ehsa_2017 <- subset(ehsa_2017, select = -c(ward.y))
colnames(ehsa_2017)[2] <- "ward"
colnames(ehsa_2017)[4] <- "p_value"

colnames(ehsa_manual_2018)[1] <- "ward"
ehsa_2018 <- merge(Geo, ehsa_manual_2018, by = "ID", all.x = TRUE)
ehsa_2018 <- subset(ehsa_2018, select = -c(ward.y))
colnames(ehsa_2018)[2] <- "ward"
colnames(ehsa_2018)[4] <- "p_value"

colnames(ehsa_manual_2019)[1] <- "ward"
ehsa_2019 <- merge(Geo, ehsa_manual_2019, by = "ID", all.x = TRUE)
ehsa_2019 <- subset(ehsa_2019, select = -c(ward.y))
colnames(ehsa_2019)[2] <- "ward"
colnames(ehsa_2019)[4] <- "p_value"

colnames(ehsa_manual_2020)[1] <- "ward"
ehsa_2020 <- merge(Geo, ehsa_manual_2020, by = "ID", all.x = TRUE)
ehsa_2020 <- subset(ehsa_2020, select = -c(ward.y))
colnames(ehsa_2020)[2] <- "ward"
colnames(ehsa_2020)[4] <- "p_value"

colnames(ehsa_manual_2021)[1] <- "ward"
ehsa_2021 <- merge(Geo, ehsa_manual_2021, by = "ID", all.x = TRUE)
ehsa_2021 <- subset(ehsa_2021, select = -c(ward.y))
colnames(ehsa_2021)[2] <- "ward"
colnames(ehsa_2021)[4] <- "p_value"

ehsa_cls_2017 <- ehsa_2017 %>% 
  mutate(cluster = case_when(
    p_value > 0.25 ~ "Not significant",
    p_value <= 0.25 & tau < 0 ~ "Low", 
    p_value <= 0.25 & tau > 0 ~ "High"
  ))

ehsa_cls_2018 <- ehsa_2018 %>% 
  mutate(cluster = case_when(
    p_value > 0.25 ~ "Not significant",
    p_value <= 0.25 & tau < 0 ~ "Low", 
    p_value <= 0.25 & tau > 0 ~ "High"
  ))

ehsa_cls_2019 <- ehsa_2019 %>% 
  mutate(cluster = case_when(
    p_value > 0.25 ~ "Not significant",
    p_value <= 0.25 & tau < 0 ~ "Low", 
    p_value <= 0.25 & tau > 0 ~ "High"
  ))

ehsa_cls_2020 <- ehsa_2020 %>% 
  mutate(cluster = case_when(
    p_value > 0.25 ~ "Not significant",
    p_value <= 0.25 & tau < 0 ~ "Low", 
    p_value <= 0.25 & tau > 0 ~ "High"
  ))

ehsa_cls_2021 <- ehsa_2021 %>% 
  mutate(cluster = case_when(
    p_value > 0.25 ~ "Not significant",
    p_value <= 0.25 & tau < 0 ~ "Low", 
    p_value <= 0.25 & tau > 0 ~ "High"
  ))

Map_2017 <- ggplot(data = ehsa_cls_2017,
                   aes(fill = cluster)) +
  
  geom_sf(lwd = 0.1, color = "black") +
  scale_fill_manual(values = c("High" = "Red",
                               "Low" = "Blue", 
                               "Not significant" = "grey")) +
  ggtitle("2017 spacetime clusters")+
  theme_void()

Map_2018 <- ggplot(data = ehsa_cls_2018,
                   aes(fill = cluster)) +
  
  geom_sf(lwd = 0.1, color = "black") +
  scale_fill_manual(values = c("High" = "Red",
                               "Low" = "Blue", 
                               "Not significant" = "grey")) +
  ggtitle("2018 spacetime clusters")+
  theme_void()

Map_2019 <- ggplot(data = ehsa_cls_2019,
                   aes(fill = cluster)) +
  
  geom_sf(lwd = 0.2, color = "black") +
  scale_fill_manual(values = c("High" = "Red",
                               "Low" = "Blue", 
                               "Not significant" = "grey")) +
  ggtitle("2019 spacetime clusters")+
  theme_void()

Map_2020 <- ggplot(data = ehsa_cls_2020,
                   aes(fill = cluster)) +
  
  geom_sf(lwd = 0.2, color = "black") +
  scale_fill_manual(values = c("High" = "Red",
                               "Low" = "Blue", 
                               "Not significant" = "grey")) +
  ggtitle("2020 spacetime clusters")+
  theme_void()

Map_2021 <- ggplot(data = ehsa_cls_2021,
                   aes(fill = cluster)) +
  
  geom_sf(lwd = 0.2, color = "black") +
  scale_fill_manual(values = c("High" = "Red",
                               "Low" = "Blue", 
                               "Not significant" = "grey")) +
  ggtitle("2021 spacetime clusters")+
  theme_void()

library(gridExtra)

grid.arrange(Map_2017)

emerging <- ehsa_manual |>
  arrange(sl, abs(tau)) |>
  slice(1:5)

emerging

# conduct Emerging Hotspot Analysis
ehsa_stc_2017 <- emerging_hotspot_analysis(
  x = bos_stc_2017,
  .var = "Cases",
  k = 1,
  nsim = 99
)

ehsa_stc_2017
colnames(ehsa_stc_2017)[1] <- "ward"

ehsa_stc_2018 <- emerging_hotspot_analysis(
  x = bos_stc_2018,
  .var = "Cases",
  k = 1,
  nsim = 99
)

ehsa_stc_2018
colnames(ehsa_stc_2018)[1] <- "ward"

ehsa_stc_2019 <- emerging_hotspot_analysis(
  x = bos_stc_2019,
  .var = "Cases",
  k = 1,
  nsim = 99
)

ehsa_stc_2019
colnames(ehsa_stc_2019)[1] <- "ward"

ehsa_stc_2020 <- emerging_hotspot_analysis(
  x = bos_stc_2020,
  .var = "Cases",
  k = 1,
  nsim = 99
)

ehsa_stc_2020
colnames(ehsa_stc_2020)[1] <- "ward"

ehsa_stc_2021 <- emerging_hotspot_analysis(
  x = bos_stc_2021,
  .var = "Cases",
  k = 1,
  nsim = 99
)

ehsa_stc_2021
summarise(ehsa_stc_2021)
colnames(ehsa_stc_2021)[1] <- "ward"
ehsa_stc_2017[ehsa_stc_2017$classification == "sporadic hotspot",]

Matrix_2017 <- ehsa_stc_2017 %>%
  select(tau, p_value)%>%
  as.matrix
barplot(Matrix_2017$tau, Matrix_2017$p_value)

count(ehsa_stc_2017, classification)
count(ehsa_stc_2018, classification)
count(ehsa_stc_2019, classification)
count(ehsa_stc_2020, classification)
count(ehsa_stc_2021, classification)
library(ggplot2)




Data_stc_2017 <- merge(Geo, ehsa_stc_2017, by = "ward", all.x = TRUE)
Data_stc_2018 <- merge(Geo, ehsa_stc_2018, by = "ward", all.x = TRUE)
Data_stc_2019 <- merge(Geo, ehsa_stc_2019, by = "ward", all.x = TRUE)
Data_stc_2020 <- merge(Geo, ehsa_stc_2020, by = "ward", all.x = TRUE)
Data_stc_2021 <- merge(Geo, ehsa_stc_2021, by = "ward", all.x = TRUE)

Ex_2017 <- subset(Data_stc_2017, select = -c(geometry))
Ex_2018 <- subset(Data_stc_2018, select = -c(geometry))
Ex_2019 <- subset(Data_stc_2019, select = -c(geometry))
Ex_2020 <- subset(Data_stc_2020, select = -c(geometry))
Ex_2021 <- subset(Data_stc_2021, select = -c(geometry))

#write.csv(Ex_2017,"C:/Users/Edwin Tarus/Documents/GIS project/Objective 2/Workspace/EHSA_2017.csv", row.names = FALSE)
#write.csv(Ex_2018,"C:/Users/Edwin Tarus/Documents/GIS project/Objective 2/Workspace/EHSA_2018.csv", row.names = FALSE)
#write.csv(Ex_2019,"C:/Users/Edwin Tarus/Documents/GIS project/Objective 2/Workspace/EHSA_2019.csv", row.names = FALSE)
#write.csv(Ex_2020,"C:/Users/Edwin Tarus/Documents/GIS project/Objective 2/Workspace/EHSA_2020.csv", row.names = FALSE)
#write.csv(Ex_2021,"C:/Users/Edwin Tarus/Documents/GIS project/Objective 2/Workspace/EHSA_2021.csv", row.names = FALSE)


Data_stc_2017

EHSA_map_2017 <-  ggplot(data = Data_stc_2017,
                         aes(fill = classification)) +
  
  geom_sf(lwd = 0.2, color = "black") +
  scale_fill_manual(values = c("consecutive coldspot" = "darkslategray4",
                               "sporadic hotspot" = "brown3", 
                               "sporadic coldspot" = "darkslategray1",
                               "no pattern detected" = "grey")) +
  ggtitle("2017 Emerging hotspots")+
  theme_void()
plot(EHSA_map_2017)

EHSA_map_2018 <-  ggplot(data = Data_stc_2018,
                         aes(fill = classification)) +
  
  geom_sf(lwd = 0.2, color = "black") +
  scale_fill_manual(values = c("sporadic hotspot" = "brown3", 
                               "sporadic coldspot" = "darkslategray1",
                               "consecutive coldspot" = "darkslategray4",
                               "oscilating coldspot" = "cyan1",
                               "new coldspot" = "darkslategray1",
                               "no pattern detected" = "grey")) +
  ggtitle("2018 Emerging Hotspots")+
  theme_void()
plot(EHSA_map_2018)

EHSA_map_2019 <-  ggplot(data = Data_stc_2019,
                         aes(fill = classification)) +
  
  geom_sf(lwd = 0.2, color = "black") +
  scale_fill_manual(values = c("consecutive coldspot" = "darkslategray4",
                               "sporadic hotspot" = "brown3", 
                               "sporadic coldspot" = "cyan3",
                               "oscilating coldspot" = "cyan1",
                               "new coldspot" = "darkslategray1",
                               "persistent coldspot" = "deepskyblue",
                               "no pattern detected" = "grey")) +
  ggtitle("2019 Emerging Hotspots")+
  theme_void()
plot(EHSA_map_2019)

EHSA_map_2020 <-  ggplot(data = Data_stc_2020,
                         aes(fill = classification)) +
  
  geom_sf(lwd = 0.2, color = "black") +
  scale_fill_manual(values = c("consecutive coldspot" = "darkslategray4",
                               "consecutive hotspot" = "brown1",
                               "sporadic hotspot" = "brown3", 
                               "sporadic coldspot" = "cyan3",
                               "new hotspot" = "red",
                               "new coldspot" = "darkslategray1",
                               "persistent coldspot" = "deepskyblue",
                               "no pattern detected" = "grey")) +
  ggtitle("2020 Emerging Hotspots")+
  theme_void()
plot(EHSA_map_2020)

EHSA_map_2021 <-  ggplot(data = Data_stc_2021,
                         aes(fill = classification)) +
  
  geom_sf(lwd = 0.2, color = "black") +
  scale_fill_manual(values = c("persistent hotspot" = "darkred",
                               "sporadic hotspot" = "brown3", 
                               "sporadic coldspot" = "darkslategray1",
                               "consecutive hotspot" = "brown1",
                               "persistent coldspot" = "deepskyblue",
                               "no pattern detected" = "grey")) +
  ggtitle("2021 Emerging Hotspots")+
  theme_void()
plot(EHSA_map_2021)





