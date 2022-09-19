# 1. 회귀분석: 단순회귀모형 -----------------------------------------------------------------
# install.packages("MASS")
# install.packages("car")
# install.packages("dplyr")
library(MASS)
library(car)
library(dplyr)
head(cats, 4)
glimpse(cats)

lm_mod <- lm(Hwt ~ Bwt, data = cats)
summary(lm_mod)

plot(cats$Hwt ~ cats$Bwt, pch = 19, col = "darkgray")
abline(lm_mod, lwd = 2)

par(mfrow = c(1, 2))
plot(lm_mod, 1)
plot(lm_mod, 2)

cats[144, ]
lm_mod$fitted[144]
lm_mod$residuals[144]

residuals(lm_mod) |> 
    durbinWatsonTest()

# 2.  회귀분석: 다중회귀모형 --------------------------------------------------------
head(state.x77, 3)
glimpse(state.x77)
# install.packages("janitor")
state <- as.data.frame(state.x77) |> 
    janitor::clean_names()
glimpse(state)

lm_mod2 <- lm(life_exp ~ ., data = state)
summary(lm_mod2)

vif(lm_mod2)

lm_mod3 <- step(lm_mod2, direction = "backward", trace = FALSE)
summary(lm_mod3)

confint(lm_mod3)
predict(lm_mod3, list(population = 4000, murder = 10.5, hs_grad = 48, frost = 100))


# 3. ARIMA 모형: 실제 사례 중심으로 -------------------------------------------------
# install.packages("fpp3")
# install.packages("readr")
# install.packages("showtext")
# install.packages("ggplot2")
# install.packages("conflicted")
library(fpp3)
library(readr)
library(showtext)
library(ggplot2)
library(ggh4x)
font_add_google(name = "Nanum Gothic", family = "nanum")
showtext_auto(enable = TRUE)
conflicted::conflict_prefer("select", "dplyr")
ggplot2::theme_set(theme_bw())

disease <- read_csv("./data/example.csv")
disease
disease |> 
    select(-Date, -N) |> 
    distinct()

disease2 <- disease |> 
    slice(-2)
disease2

disease3 <- disease2 |> 
    mutate(Date = yearmonth(Date)) |> 
    as_tsibble(key = c(Sex, Group), index = Date)
disease3

disease4 <- disease3 |> 
    fill_gaps(N = 121L, .full = TRUE)
disease4

mod <- disease4 |> 
    model(arima = ARIMA(N))
mod

f <- mod %>% 
    forecast(h = "15 months") %>% 
    hilo(level = c(80, 95)) %>% 
    unpack_hilo(c("80%", "95%"))   # 신뢰구간 값 열로 추출
head(f, 3)

historic <- bind_rows(
    disease4 %>% 
        mutate(Type = "과거 실제값") %>%
        as_tibble(),
    mod %>% 
        fitted %>%  
        mutate(Type = "모형 적합값") %>% 
        as_tibble %>% 
        rename(N = .fitted) %>% 
        select(Date, Sex, Group, N, Type)
)

fore <- f %>% 
    mutate(Type = "모형 예측값", N = .mean) %>% 
    as_tibble %>% 
    select(Date, Sex, Group, N, Type)

ggplot() +
    geom_line(data = historic,
              aes(x = Date, y = N, col = Type)) + # 과거 실제값, 모형 적합값
    geom_line(data = fore,
              aes(x = Date, y = N, col = Type)) + # 모형 예측값
    geom_ribbon(data = f, # 80% 신뢰구간
                aes(x = Date, ymin = `80%_lower`, ymax = `80%_upper`),
                fill = "skyblue", alpha = 0.25) +
    geom_ribbon(data = f, # 95% 신뢰구간
                aes(x = Date, ymin = `95%_lower`, ymax = `95%_upper`),
                fill = "skyblue", alpha = 0.25/2) +
    scale_color_manual(values = c("tomato", "blue", "paleturquoise3"), name = "") +
    theme(
        text = element_text(family = "nanum", size = 12),
        legend.position = "bottom") +
    labs(x = " ",
         y = "N") +
    facet_nested_wrap(vars(Sex, Group), scales = "free",
                      ncol = 4)