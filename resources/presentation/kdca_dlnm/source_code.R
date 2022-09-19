# 패키지 설치
# install.packages(c("tidyverse", "dlnm", "splines", "stringr", "lubridate"))

# 패키지 로딩
library(tidyverse)
library(dlnm)
library(splines)
library(stringr)
library(lubridate)
ggplot2::theme_set(theme_bw())

# vignette('dlnmOverview')

chicagoNMMAPS |> 
    skimr::skim()

# 데이터 로딩
chicago <- chicagoNMMAPS |> 
    as_tibble() |> 
    select(date, time, year, pm10, temp, death)
glimpse(chicago)
head(chicago)

chicago |> 
    filter(is.na(pm10)) |> 
    nrow()

# 데이터 출력
head(chicago)

# EDA
chicago |> 
    ggplot(aes(x = date, y = pm10)) + 
    geom_line()

# Create cross-basis
cb_pm <- crossbasis(chicago$pm10,
                    lag = 14,
                    argvar = list(fun = "ns", df = 3),
                    arglag = list(fun = "ns", df = 3))
num_year <- chicago |> 
    select(year) |> 
    unique() |> 
    nrow()
mod <- glm(death ~ cb_pm + temp + ns(time, 7*num_year), 
           family = quasipoisson(), data = chicago)
summary(mod)
pred_pm <- crosspred(cb_pm, mod, 
                     cen = chicago$pm10 |> 
                         median(na.rm = T),
                     at = 10:80, by = 1)

# 시각화 - 3D plot
plot(pred_pm, xlab = "PM10", zlab = "RR",
     theta = 210, phi = 30, lphi = 30, border = "gray40")

# 시각화 - Contour plot
plot(pred_pm, "contour", xlab = "PM10", ylab = "RR",
     key.title = title("RR"))

# 시각화 - Overall cumulative association plot
plot(pred_pm, "overall", col = "tomato", lwd = 2, 
     xlab = "PM10", ylab = "RR")
rug(chicago$pm10, quiet = TRUE)

# 시각화 - High effect(vs 90th quantile)
plot(pred_pm, "slices",
     var = chicago$pm10 |> 
         quantile(0.9, na.rm = TRUE) |> 
         round(0),
     col = "tomato", lwd = 2, ylab = "RR", 
     main = "High PM10 effect (vs 90th quantile)")

# 시각화 - Low effect(vs 10th quantile)
plot(pred_pm, "slices",
     var = chicago$pm10 |> 
         quantile(0.1, na.rm = TRUE) |> 
         round(0),
     col = "tomato", lwd = 2, ylab = "RR", 
     main = "Low PM10 effect (vs 10th quantile)")
