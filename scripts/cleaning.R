


# Exercise 01 ----------------------
ookla <- read_delim("data/colombia_connectivity_wide.csv")

# 1. Remove duplicates

ookla <- as.data.table(ookla)
ookla <- distinct(ookla)

# 2. Pivot the data

ookla.longer <- melt(ookla, measure = list(paste0("avg_d_kbps_0",c(1,4)),
                                paste0("avg_u_kbps_0",c(1,4)),
                                paste0("avg_lat_ms_0",c(1,4)),
                                paste0("tests_0",c(1,4)),
                                paste0("devices_0",c(1,4))),
     value.name = c("avg_d_kbps",
                    "avg_u_kbps",
                    "tests",
                    "avg_lat_ms",
                    "devices"))
setnames(ookla.longer,c("variable"),c("quarter"))

# Exercise 02 ---------------------------------------------------------

open <- read_delim("data/colombia_infrastructure_lng.csv")
open <- as.data.table(open)


# reshape
open.wider <- tidyr::pivot_wider(data = open,id_cols = starts_with("ADM"),names_from = "amenities")



# Challenge 1 ------------------------------------

ookla %>%
  fgroup_by(ADM2_ES) %>%
  fsummarise(download = fmean(avg_d_kbps_04,na.rm = T)) %>%
  arrange(desc(download)) %>%
  filter(!is.na(download)) %>%
  top_n(1)

ookla.longer %>%
  filter(quarter == 2) %>%
  fgroup_by(ADM2_ES) %>%
  fsummarise(download = fmean(avg_d_kbps,na.rm = T)) %>%
  arrange(desc(download)) %>%
  filter(!is.na(download)) %>%
  top_n(1)

# Challenge 2 ------------------------------------



open %>%
  filter(amenities %in% c("school","college","university")) %>%
  fgroup_by(ADM2_ES) %>%
  fsummarise(count_schools = fsum(value,na.rm = T)) %>%
  # filter(amenities == "school") %>%
  arrange(desc(count_schools)) %>%
  top_n(2)

open.wider %>%
  mutate(total = rowSums(select(.,school,university,college))) %>%
  summarise(count_schools = mean(total,na.rm = T),.by = c("ADM2_ES")) %>%
  arrange(desc(count_schools)) %>%
  top_n(2)


# Cleaning exercises ---------------------------------------------

decleaned <- read_delim("data/colombia_connectivity_decleaned.csv")
decleaned <- as.data.table(decleaned)

# Dropping unused variables
decleaned <- decleaned[,-c("id_test_data")]

# Variables types

str(decleaned)
# Missingness
apply(decleaned, 2, function(x) table(is.na(x)))
decleaned[,total := avg_d_kbps + avg_u_kbps + avg_lat_ms + tests + devices]
decleaned <- decleaned[!is.na(total)]

# Special characters

decleaned[, quadkey := stri_trans_general(quadkey,id = 'latin-ascii')]
decleaned[, ADM0_PC := stri_trans_general(ADM0_PC,id = 'latin-ascii')]
decleaned[, ADM0_ES := stri_trans_general(ADM0_ES,id = 'latin-ascii')]
decleaned[, ADM1_PC := stri_trans_general(ADM1_PC,id = 'latin-ascii')]
decleaned[, ADM1_ES := stri_trans_general(ADM1_ES,id = 'latin-ascii')]
decleaned[, ADM2_PC := stri_trans_general(ADM2_PC,id = 'latin-ascii')]
decleaned[, ADM2_ES := stri_trans_general(ADM2_ES,id = 'latin-ascii')]


decleaned <- decleaned[,-c("total")]

# labeling variables
label.decleaned <- set_variable_labels(.data = decleaned,.labels = c("Tile id","Country ISO Code","Country name",
                                                  "State Code","State Name","Municipality code",
                                                  "Municipality name","Type of connection",
                                                  "Quarter of observation","Average Dowload speed",
                                                  "Average Upload speed","Average latency metric",
                                                  "Number of tests done","Number of devices"))



codebook <- purrr::map_df(label.decleaned, function(x) attributes(x)$label) %>%
  tidyr::gather(key = Code, value = Label)

codebook <- dplyr::bind_cols(codebook,sapply(label.decleaned, class) %>% as.data.frame(.))
colnames(codebook) <- c("key","value","type")

write_csv(x = codebook,file = "outputs/codebook.csv")

