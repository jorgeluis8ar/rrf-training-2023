
# 1 Loading the data -------------------------------------------

ookla <- read_delim("data/colombia_connectivity_cleaned.csv")
ookla <- as.data.table(ookla)

open <- read_delim("data/colombia_infrastructure_cleaned.csv")
open <- as.data.table(open)



ookla[, avg_d_Mbps := avg_d_kbps/1000]
ookla[, avg_u_Mbps := avg_u_kbps/1000]


# WInsorization
ggplot(data = ookla) + geom_density(aes(avg_d_Mbps))
ggplot(data = ookla) + geom_density(aes(avg_u_Mbps))

winsor_function <- function(dataset, var, min = 0.00, max = 0.99){
  var_sym <- sym(var)

  percentiles <- quantile(
    dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
  )

  min_percentile <- percentiles[1]
  max_percentile <- percentiles[2]

  dataset %>%
    mutate(
      !!paste0(var, "_winsorized") := case_when(
        is.na(!!var_sym) ~ NA_real_,
        !!var_sym <= min_percentile ~ percentiles[1],
        !!var_sym >= max_percentile ~ percentiles[2],
        TRUE ~ !!var_sym
      )
    )
}

ookla <- ookla %>% winsor_function(var = "avg_d_Mbps")
ookla <- ookla %>% winsor_function(var = "avg_u_Mbps")



aggre.df <- ookla %>%
  fgroup_by(trimester,ADM1_ES,ADM2_ES,ADM2_PC) %>%
  fsummarise(upload = fmean(avg_u_Mbps_winsorized,na.rm = T),
             dowload = fmean(avg_d_Mbps_winsorized,na.rm = T)) %>%
  merge(x = ., y = open[,c("ADM2_PC","college","clinic","university","school","hospital")],
        by = "ADM2_PC",all.x = T) %>%
  mutate(next.upload = lag(upload, order_by = ADM2_PC),
         next.upload = ifelse(trimester == 1,NA,next.upload),
         next.dowload = lag(dowload, order_by = ADM2_PC),
         next.dowload = ifelse(trimester == 1,NA,next.dowload),
         dif.upload = upload - next.upload,
         dif.dowload = dowload - next.dowload) %>%
  select(-c(next.dowload,next.upload))

ggplot(data = aggre.df) + geom_density(aes(dif.upload))
ggplot(data = aggre.df) + geom_density(aes(dif.dowload))



# Last exercise ------------------------------------

# 1. Reading date

muni <- read_delim("data/municipality_database.csv")
muni <- as.data.table(muni)

state <- read_delim("data/state_database.csv")
state <- as.data.table(muni)

# 2. Exporting summary statistics
stargazer(muni,out = "outputs/municipality_descriptive.tex")
stargazer(state,out = "outputs/state_descriptive.tex")

# 3. Histograms and boxplots
state %>%
  select(college:hospital) %>%
  tidyr::pivot_longer(cols = everything()) %>%
  ggplot(aes(x = name,y = value, fill = name)) +
  geom_boxplot() +
  labs(title = "Boxplot of social infraestructure",
       x = "Variables", y = " Number") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()


hist.download.plot <- ggplot(data = muni) +
  geom_histogram(aes(x = avg_d_mbps_winsorized),color="black", fill="lightgrey") +
  labs(x = "Average Upload Speed", y = "Count", caption = "Data has been winsorized at the 99%") +
  scale_x_continuous(labels = scales::label_number(suffix = " Mb")) +
  scale_y_continuous(breaks = seq(0,500,50)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        axis.title = element_text(size = 16, color = "black"),axis.text = element_text(size = 14, color = "black"))

hist.upload.plot <- ggplot(data = muni) +
  geom_histogram(aes(x = avg_u_mbps_winsorized),color="black", fill="lightgrey") +
  labs(x = "Average Download Speed", y = "Count", caption = "Data has been winsorized at the 99%") +
  scale_x_continuous(labels = scales::label_number(suffix = " Mb")) +
  scale_y_continuous(breaks = seq(0,500,50)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        axis.title = element_text(size = 16, color = "black"),axis.text = element_text(size = 14, color = "black"))

ggsave(plot = hist.download.plot,
       filename = "outputs/histograms/dowload.pdf",
       device = "pdf",width = 12,height = 6,units = "in")

ggsave(plot = hist.upload.plot,
       filename = "outputs/histograms/upload.pdf",
       device = "pdf",width = 12,height = 6,units = "in")


regs.speeds <- feols(c(avg_d_mbps_winsorized,avg_u_mbps_winsorized) ~ school,data = muni,vcov = ~ ADM1_PC)
etable(regs.speeds)

myDict <- c("(Intercept)" = "Intercept","school" = "Number of schools")

etable(regs.speeds,dict = myDict,tex = T,file = "outputs/regressions/regressions.tex")

geom.smooth.plot <- muni %>%
ggplot(aes(x = school, y = avg_d_mbps_winsorized)) +
  geom_point() +
  geom_smooth(method = "lm")


ggsave(plot = geom.smooth.plot,
       filename = "outputs/geom_plot_smooth.pdf",
       device = "pdf",width = 12,height = 6,units = "in")

geom.bar.plot <- muni %>%
filter(trimester == 4) %>%
  arrange(desc(abs(avg_d_mbps_change))) %>%
  filter(!is.na(avg_d_mbps_change)) %>%
  top_n(10) %>%
  select(ADM2_ES, avg_d_mbps_change,avg_d_mbps) %>%
  ggplot(aes(y = reorder(ADM2_ES,avg_d_mbps_change) , x = avg_d_mbps_change)) +
  geom_col(fill = "lightgrey") +
  geom_text(aes(label = paste0(round(avg_d_mbps,2)," Mb")),hjust = -0.3) +
  scale_x_continuous(breaks = seq(0,2500,250),labels = scales::label_number(suffix = " Mb",big.mark = ",")) +
  labs(x = "Absolute Difference in Average Download Speed",
       y = "Municipality") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 14, color = "black"))



ggsave(plot = geom.bar.plot,
       filename = "outputs/geom_bar.pdf",
       device = "pdf",width = 12,height = 6,units = "in")



