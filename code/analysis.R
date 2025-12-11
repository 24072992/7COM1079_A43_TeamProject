#dataset analyse and cleaned
df <- read_csv("RA2019_A2.csv")
View(df)
names(df)
df_clean <- df[, c("States/UTs",
                   "State/UT-Wise Total Number of Road Accidents during 2019 - Numbers",
                   "Total Number of Accidents Per Lakh Population - 2019")]

names(df_clean) <- c("State", "Total_Accidents_2019", "Rate_Per_Lakh_2019")
df_clean <- df_clean[df_clean$State != "Total", ]
View(df_clean)
