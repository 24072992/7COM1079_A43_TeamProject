#dataset analyse and cleaned
df <- read_csv("RA2019_A2.csv")
View(df)
names(df)
df_clean <- df[, c("States/UTs",
                   "State/UT-Wise Total Number of Road Accidents during 2019 - Numbers",
                   "Total Number of Accidents Per Lakh Population - 2019")]

names(df_clean) <- c("State", "Total_Accidents_2019", "Rate_Per_Lakh_2019")
df_clean <- df_clean[df_clean$State != "Total", ]
df_clean <- df_clean[!is.na(df_clean$Rate_Per_Lakh_2019), ]
View(df_clean)

#Total Number Of Road Accidents Graph
png("Histogram_Total_Accidents_2019.png", width = 800, height = 600)
hist(df_clean$Total_Accidents_2019,
     main = "Histogram of Total Number of Road Accidents (2019)",
     xlab = "Total Accidents (Count)",
     ylab = "Number of States/UTs",
     col = "red",
     border = "black")
     dev.off()

#Total number of road accidents per lakh population graph

png("Histogram_Accident_Rate_2019.png", width = 800, height = 600)
h <- hist(df_clean$Rate_Per_Lakh_2019, 
          main = "Histogram of Accident Rate per Lakh Population - 2019",
          xlab = "Accident Rate per Lakh Population",
          ylab = "Number of States/UTs",
          col = "lightgreen",
          border = "black")
        

xfit <- seq(min(df_clean$Rate_Per_Lakh_2019),
            max(df_clean$Rate_Per_Lakh_2019),
            length = 100)

yfit <- dnorm(xfit, 
              mean = mean(df_clean$Rate_Per_Lakh_2019),
              sd = sd(df_clean$Rate_Per_Lakh_2019))

yfit <- yfit * diff(h$mids[1:2]) * length(df_clean$Rate_Per_Lakh_2019)
lines(xfit, yfit, col = "red", lwd = 2)
dev.off()

#scatterplot for analysis
png("Scatterplot_Accidents_vs_Rate_2019.png", width = 800, height = 600)
plot(df_clean$Total_Accidents_2019,
     df_clean$Rate_Per_Lakh_2019,
     main = "Scatterplot: Total Accidents vs Accident Rate (2019)",
     xlab = "Total Number of Road Accidents (2019)",
     ylab = "Accident Rate per Lakh Population (2019)",
     pch = 19,
     col = "blue")
abline(lm(df_clean$Rate_Per_Lakh_2019 ~ df_clean$Total_Accidents_2019),
       col = "red", lwd = 2)
dev.off()

# Run Spearman correlation test
# This checks the relationship between total accidents and accident rate per lakh population
# Spearman is used because the data is not normally distributed

cor.test(df_clean$Total_Accidents_2019,
         df_clean$Rate_Per_Lakh_2019,
         method = "spearman")

