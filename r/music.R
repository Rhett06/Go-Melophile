library(readr)
library(effsize)
library(ggplot2)
library(ggpubr)
library(FSA)
library(rstatix)

spotify = read.csv("data/spotify.csv")
yt = read.csv("data/yt.csv")
download = read.csv("data/download.csv")
wifi = read.csv("data/wifi.csv")
v1 = read.csv("data/v1.csv")
v2 = read.csv("data/v2.csv")
v3 = read.csv("data/v3.csv")
low = read.csv("data/low.csv")
normal = read.csv("data/normal.csv")
high = read.csv("data/high.csv")
veryhigh = read.csv("data/veryhigh.csv")

spotify_df = data.frame(spotify)
yt_df = data.frame(yt)
download_df = data.frame(download)
wifi_df = data.frame(wifi)
v1_df = data.frame(v1)
v2_df = data.frame(v2)
v3_df = data.frame(v3)
low_df = data.frame(low)
normal_df = data.frame(normal)
high_df = data.frame(high)
veryhigh_df = data.frame(veryhigh)

############################# box plot #############################
# ------- wifi vs. download -------
wifi_download = rbind(download_df, wifi_df)
wifi_download$app = as.factor(wifi_download$app)
conn_box <- ggplot(wifi_download, aes(x=app, y=value, fill=variable)) +
  geom_boxplot(position=position_dodge(1)) + 
  scale_fill_brewer(palette="Pastel2", name = "Connection Type", 
                    labels = c("Downloaded File Playing", "Wi-Fi Streaming")) +
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size= 15, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12)) +
  ylab("Energy Consumption (J)") 


# ------- volume -------
volume = rbind(v1_df, v2_df, v3_df)
volume$app = as.factor(volume$app)
volume_box <- ggplot(volume, aes(x=app, y=value, fill=variable)) +
  geom_boxplot(position=position_dodge(1)) + 
  scale_fill_brewer(palette="Pastel2", name = "Sound Volume", 
                    labels = c("Low", "Medium", "High")) +
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size= 15, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12)) +
  ylab("Energy Consumption (J)") 


# ------- audio quality -------
quality = rbind(low_df, normal_df, high_df, veryhigh_df)
quality$app = as.factor(quality$app)
quality_box <- ggplot(quality, aes(x=app, y=value, fill=variable)) +
  geom_boxplot(position=position_dodge(1)) + 
  scale_fill_brewer(palette="Pastel2", name = "Audio Quality", 
                    labels = c("High", "Low", "Normal", "Very High")) +
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size= 15, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12)) +
  ylab("Energy Consumption (J)")

ggsave("conn_box.png", conn_box, height = 5, width = 7, dpi = 320)
ggsave("volume_box.png", volume_box, height = 5, width = 7, dpi = 320)
ggsave("quality_box.png", quality_box, height = 5, width = 7, dpi = 320)

############################# density plot #############################
# ------- whole dataset -------
app_df = rbind(download_df, wifi_df, v1_df, v2_df, v3_df, low_df, normal_df, high_df, veryhigh_df)
app_df$app = as.factor(app_df$app)
all_density <- ggplot(app_df, aes(x=value, fill=app)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        axis.text = element_text(size = 8), 
        axis.title = element_text(size = 10),
        legend.position = "bottom") +
  xlab("Energy Consumption (J)") +
  ylab("Density")

ggsave("all_density.png", all_density, height = 5, width = 7, dpi = 320)


# ------- download vs. Wi-Fi -------
wifi_df$app = as.factor(wifi_df$app)
wifi_density <- ggplot(wifi_df, aes(x=value, fill=app), size=I(0.5)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        plot.title = element_text(size= 9, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  labs(title="Wi-Fi Streaming") + 
  xlab("Energy Consumption (J)") +
  ylab("Density")

download_df$app = as.factor(download_df$app)
download_density <- ggplot(download_df, aes(x=value, fill=app), size=I(0.5)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        plot.title = element_text(size= 9, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  labs(title="Downloaded File Playing") + 
  xlab("Energy Consumption (J)") +
  ylab("Density")


# ------- sound volume -------
v1_df$app = as.factor(v1_df$app)
v1_density <- ggplot(v1_df, aes(x=value, fill=app), size=I(0.5)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        plot.title = element_text(size= 9, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  labs(title="Low Sound Volume") + 
  xlab("Energy Consumption (J)") +
  ylab("Density")

v2_df$app = as.factor(v2_df$app)
v2_density <- ggplot(v2_df, aes(x=value, fill=app), size=I(0.5)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        plot.title = element_text(size= 9, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  labs(title="Medium Sound Volume") + 
  xlab("Energy Consumption (J)") +
  ylab("Density")

v3_df$app = as.factor(v3_df$app)
v3_density <- ggplot(v3_df, aes(x=value, fill=app), size=I(0.5)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        plot.title = element_text(size= 9, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  labs(title="High Sound Volume") + 
  xlab("Energy Consumption (J)") +
  ylab("Density")


# ------- audio quality -------
low_df$app = as.factor(low_df$app)
low_density <- ggplot(low_df, aes(x=value, fill=app), size=I(0.5)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        plot.title = element_text(size= 9, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  labs(title="Low Audio Quality") + 
  xlab("Energy Consumption (J)") +
  ylab("Density")

normal_df$app = as.factor(normal_df$app)
normal_density <- ggplot(normal_df, aes(x=value, fill=app), size=I(0.5)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        plot.title = element_text(size= 9, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  labs(title="Normal Audio Quality") + 
  xlab("Energy Consumption (J)") +
  ylab("Density")

high_df$app = as.factor(high_df$app)
high_density <- ggplot(high_df, aes(x=value, fill=app), size=I(0.5)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        plot.title = element_text(size= 9, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  labs(title="High Audio Quality") + 
  xlab("Energy Consumption (J)") +
  ylab("Density")

veryhigh_df$app = as.factor(veryhigh_df$app)
veryhigh_density <- ggplot(veryhigh_df, aes(x=value, fill=app), size=I(0.5)) +
  geom_density(alpha=0.4, adjust=2) + 
  scale_fill_brewer(palette="Pastel2") +
  theme_minimal() + 
  theme(legend.title = element_blank(),        
        plot.title = element_text(size= 9, face="bold", hjust = 0.5), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  labs(title="Very High Audio Quality") + 
  xlab("Energy Consumption (J)") +
  ylab("Density")

density_combined <- ggarrange(wifi_density, download_density, v1_density, 
                              v2_density, v3_density, low_density, 
                              normal_density, high_density, veryhigh_density,
                              ncol = 3,
                              nrow = 3, 
                              legend = c("bottom"),
                              common.legend = TRUE
)

ggsave("density_combined.png", density_combined, height = 5, width = 7, dpi = 320)


############################# qq plot #############################
# ------- whole dataset -------
all_qq <- qplot(sample=value, data=app_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5),
        axis.title = element_text(size = 9), 
        legend.position = "bottom")

ggsave("all_qq.png", all_qq, height = 5, width = 7, dpi = 320)


# ------- download vs. Wi-Fi -------
wifi_qq <- qplot(sample=value, data=wifi_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5),
        axis.title = element_text(size = 9)) +
  labs(title="Wi-Fi Streaming")

download_qq <- qplot(sample=value, data=download_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5)) +
  labs(title="Downloaded File Playing")


# ------- sound volume -------
v1_qq <- qplot(sample=value, data=v1_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5)) +
  labs(title="Low Volume")

v2_qq <- qplot(sample=value, data=v2_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5)) +
  labs(title="Medium Volume")

v3_qq <- qplot(sample=value, data=v3_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5)) +
  labs(title="High Volume")


# ------- audio quality -------
low_qq <- qplot(sample=value, data=low_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5)) +
  labs(title="Low Audio Quality")

normal_qq <- qplot(sample=value, data=normal_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5)) +
  labs(title="Normal Audio Quality")

high_qq <- qplot(sample=value, data=high_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5)) +
  labs(title="High Audio Quality")

veryhigh_qq <- qplot(sample=value, data=veryhigh_df, color=app, size=I(0.5)) + 
  scale_color_brewer(palette="Pastel2") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size=9, face="bold", hjust = 0.5)) +
  labs(title="Very High Audio Quality")

qq_combined <- ggarrange(wifi_qq, download_qq, v1_qq, 
          v2_qq, v3_qq, low_qq, 
          normal_qq, high_qq, veryhigh_qq,
          ncol = 3,
          nrow = 3, 
          legend = c("bottom"),
          common.legend = TRUE
)

ggsave("qq_combined.png", qq_combined, height = 5, width = 7, dpi = 320)


########################## statistics  ##########################

l_yt = c()
l_sp = c()
for (i in yt[,-1]){
  l_yt = c(l_yt, i[1:30])
}
for (i in spotify[,-1]){
  l_sp = c(l_sp, i[1:30])
}

summary(yt[1:30,-1])
summary(spotify[1:30,-1])
summary(l_yt)  # yt music
summary(l_sp)  # spotify 

# ------- download -------
summary(download_df[download_df$app == "Spotify", ][ , "value"])
summary(download_df[download_df$app == "YouTube Music", ][ , "value"])

# ------- Wi-Fi -------
summary(wifi_df[wifi_df$app == "Spotify", ][ , "value"])
summary(wifi_df[wifi_df$app == "YouTube Music", ][ , "value"])

# ------- v1 -------
summary(v1_df[v1_df$app == "Spotify", ][ , "value"])
summary(v1_df[v1_df$app == "YouTube Music", ][ , "value"])

# ------- v2 -------
summary(v2_df[v2_df$app == "Spotify", ][ , "value"])
summary(v2_df[v2_df$app == "YouTube Music", ][ , "value"])

# ------- v3 -------
summary(v3_df[v3_df$app == "Spotify", ][ , "value"])
summary(v3_df[v3_df$app == "YouTube Music", ][ , "value"])

# ------- low -------
summary(low_df[low_df$app == "Spotify", ][ , "value"])
summary(low_df[low_df$app == "YouTube Music", ][ , "value"])

# ------- normal -------
summary(normal_df[normal_df$app == "Spotify", ][ , "value"])
summary(normal_df[normal_df$app == "YouTube Music", ][ , "value"])

# ------- high -------
summary(high_df[high_df$app == "Spotify", ][ , "value"])
summary(high_df[high_df$app == "YouTube Music", ][ , "value"])

# ------- veryhigh -------
summary(veryhigh_df[ , "value"])

########################## shapiro test ##########################
shapiro.test(app_df[app_df$app=="YouTube Music",]$value)
shapiro.test(app_df[app_df$app=="Spotify",]$value)

# ------- Wi-Fi vs download -------
shapiro.test(wifi_df[wifi_df$app=="YouTube Music",]$value)
shapiro.test(wifi_df[wifi_df$app=="Spotify",]$value)
shapiro.test(download_df[download_df$app=="YouTube Music",]$value)
shapiro.test(download_df[download_df$app=="Spotify",]$value)


#------- volume ------- 
shapiro.test(v1_df[v1_df$app=="YouTube Music",]$value)
shapiro.test(v1_df[v1_df$app=="Spotify",]$value)
shapiro.test(v2_df[v2_df$app=="YouTube Music",]$value)
shapiro.test(v2_df[v2_df$app=="Spotify",]$value)
shapiro.test(v3_df[v3_df$app=="YouTube Music",]$value)
shapiro.test(v3_df[v3_df$app=="Spotify",]$value)


# ------- audio quality -------
shapiro.test(low_df[low_df$app=="YouTube Music",]$value)
shapiro.test(low_df[low_df$app=="Spotify",]$value)
shapiro.test(normal_df[normal_df$app=="YouTube Music",]$value)
shapiro.test(normal_df[normal_df$app=="Spotify",]$value)
shapiro.test(high_df[high_df$app=="YouTube Music",]$value)
shapiro.test(high_df[high_df$app=="Spotify",]$value)
shapiro.test(veryhigh_df[veryhigh_df$app=="Spotify",]$value)


########################## Mann-Whitney test ##########################
all_wilcox <- wilcox.test(app_df[app_df$app=="YouTube Music",]$value, 
            app_df[app_df$app=="Spotify",]$value)

yt_conn_wilcox <- wilcox.test(wifi_df[wifi_df$app=="YouTube Music",]$value, 
            download_df[download_df$app=="YouTube Music",]$value)

spotify_conn_wilcox <- wilcox.test(wifi_df[wifi_df$app=="Spotify",]$value, 
            download_df[download_df$app=="Spotify",]$value)

# -------- cliff's delta --------
cliff.delta(app_df[app_df$app=="YouTube Music",]$value, 
            app_df[app_df$app=="Spotify",]$value)

cliff.delta(wifi_df[wifi_df$app=="YouTube Music",]$value, 
            download_df[download_df$app=="YouTube Music",]$value)

cliff.delta(wifi_df[wifi_df$app=="Spotify",]$value, 
            download_df[download_df$app=="Spotify",]$value)

########################## Kruskal-Wallis test ##########################
d = rbind(v1_df, v2_df, v3_df)
d$variable = as.factor(d$variable)
yt_volume_kruskal <- kruskal.test(value~variable, data=d[d$app=="YouTube Music",])
spotify_volume_kruskal <- kruskal.test(value~variable, data=d[d$app=="Spotify",])

# -------- Dunn test --------
dunn_test(value~variable, data=d[d$app=="YouTube Music",], p.adjust.method = "bonferroni") 
dunn_test(value~variable, data=d[d$app=="Spotify",], p.adjust.method = "bonferroni") 

# -------- effect size --------
kruskal_effsize(value~variable, data=d[d$app=="Spotify",])
kruskal_effsize(value~variable, data=d[d$app=="YouTube Music",])

d = rbind(low_df, normal_df, high_df, veryhigh_df)
d$variable = as.factor(d$variable)
yt_quality_kruskal <- kruskal.test(value~variable, data=d[d$app=="YouTube Music",])
spotify_quality_kruskal <- kruskal.test(value~variable, data=d[d$app=="Spotify",])

# -------- Dunn test --------
dunn_test(value~variable, data=d[d$app=="Spotify",], p.adjust.method = "bonferroni") 
dunn_test(value~variable, data=d[d$app=="YouTube Music",], p.adjust.method = "bonferroni") 

# -------- effect size --------
kruskal_effsize(value~variable, data=d[d$app=="Spotify",])
kruskal_effsize(value~variable, data=d[d$app=="YouTube Music",])


########################## Benjamini–Hochberg procedure ##########################
pvalues = c(all_wilcox$p.value, 
            spotify_conn_wilcox$p.value, 
            yt_conn_wilcox$p.value, 
            spotify_volume_kruskal$p.value, 
            yt_volume_kruskal$p.value, 
            spotify_quality_kruskal$p.value, 
            yt_quality_kruskal$p.value)

p.adjust(pvalues, "BH")






