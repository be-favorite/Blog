library(fpp3)

## 원자료
tourism |>
    filter(
        Region == "Adelaide Hills", Purpose == "Visiting"
    ) |> 
    model(
        STL(Trips ~ trend(window = 7) +
                season(window = "periodic"),
            robust = TRUE)) |>
    components() |>
    autoplot()

tourism |>
    filter(
        Region == "Adelaide Hills", Purpose == "Visiting"
    ) |> 
    ggplot() +
    geom_boxplot(aes(x = Region, y = Trips))

tourism |>
    filter(
        Region == "Adelaide Hills", Purpose == "Visiting"
    ) |> 
    ggplot() +
    geom_boxplot(aes(x = Region, y = Trips), coef = 3)

tourism |>
    filter(
        Region == "Adelaide Hills", Purpose == "Visiting"
    ) |> 
    features(Trips, feat_stl)


## 이상점 탐지 및 대치(이전 날 값으로)
tourism |> 
    filter(
        Region == "Adelaide Hills", Purpose == "Visiting"
    ) |> 
    mutate(Trips = ifelse(Trips > 60, NA, Trips)) |> 
    tidyr::fill(Trips, .direction = "down") |> 
    model(
        STL(Trips ~ trend(window = 7) +
                season(window = "periodic"),
            robust = TRUE)) |>
    components() |>
    autoplot()

tourism |> 
    filter(
        Region == "Adelaide Hills", Purpose == "Visiting"
    ) |> 
    mutate(Trips = ifelse(Trips > 60, NA, Trips)) |> 
    tidyr::fill(Trips, .direction = "down") |> 
    features(Trips, feat_stl)

## A simple example of change points detection
library(changepoint)
set.seed(100)
y = c(rnorm(100, 0, 1), rnorm(100, 5, 1), rnorm(100, 2, 1))
ansmean = cpt.mean(y, method = "PELT")
plot(ansmean, cpt.col = "red")

