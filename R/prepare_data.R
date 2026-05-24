#PRELIMINARY STUFFS ON DATA

#load data
data <- read_excel("data_sample.xlsx")

#geographical borders of municipalities
mun <- esp_get_munic()

#transform the variables to character type for merging
mun$LAU_CODE <- as.character(mun$LAU_CODE)
data$COD_MUNI <- as.character(data$COD_MUNI)

#new dataset with data and geographical borders
dataset <- mun %>%
  left_join(data, by = c("LAU_CODE" = "COD_MUNI")) %>%
  select(-NOM_MUNI, -COD_CCAA)

dataset <- dataset %>%
  select(-cmun)

dataset <- dataset %>%
  rename(
    COD_CCAA = codauto,
    NOM_CCAA = ine.ccaa.name,
    COD_PRO = cpro,
    NOM_PROV = ine.prov.name,
    NOM_MUNI = name,
    COD_MUNI = LAU_CODE
  )

#clean the workspace
rm(mun, data)