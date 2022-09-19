install.packages("dplyr")
install.packages("survival")
install.packages("survminer")

library(dplyr)
library(survival)
library(survminer)

surv_lung <- Surv(lung$time, lung$status)[1:10]
head(surv_lung)

method_km <- survfit(Surv(time, status) ~ 1, data = lung)
glimpse(method_km)

ggsurvplot(method_km,  xlab = "Days", ylab = "Overall survival probability")

summary(method_km, times = 365.25)

plot_main <- 
    ggsurvplot(
        data = lung, 
        fit = method_km,
        xlab = "Months",
        legend = "none",
        xscale = 30.4,
        break.x.by = 182.4, 
        risk.table = TRUE,
        risk.table.y.text = FALSE)
plot1 <- plot_main
plot1$plot <- plot1$plot + 
    geom_segment(x = 365.25, xend = 365.25, y = -0.05, yend = 0.4092416, 
                 size = 1.5) +
    geom_segment(x = 365.25, xend = -40, y = 0.4092416, yend = 0.4092416,
                 size = 1.5, 
                 arrow = arrow(length = unit(0.2, "inches")))

method_km

plot2 <- plot_main
plot2$plot <- plot2$plot + 
    geom_segment(x = -45, xend = 310, y = 0.5, yend = 0.5,  size = 1.5) +
    geom_segment(x = 310, xend = 310, y = 0.5, yend = -0.03, size = 1.5, 
                 arrow = arrow(length = unit(0.2, "inches")))
plot2

method_km2 <- survfit(Surv(time, status) ~ sex, data = lung)
method_km2

ggsurvplot(method_km2, 
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

mod_cox <- coxph(Surv(time, status) ~ sex, data = lung)
mod_cox

install.packages("broom")
broom::tidy(mod_cox, exp = TRUE)

install.packages("gtsummary")
gtsummary::tbl_regression(mod_cox, exp = TRUE)

mod_cox |> 
    cox.zph() |> 
    ggcoxzph()

mod_hr <- survfit(Surv(time, status) ~ sex, data = lung)
ggsurvplot(data = lung, 
           fit = mod_hr,
           xlab = "Months",
           xscale = 30.4,
           break.x.by = 182.4,
           fun = "cumhaz",
           legend.title = "",
           legend.labs = c("Male", "Female"),
           legend = "bottom", 
           risk.table = TRUE,
           risk.table.y.text = FALSE)

install.packages("SemiCompRisks")
data(BMT, package = "SemiCompRisks")
BMT2 <- BMT |> 
    select(T1, delta1, TA, deltaA)
glimpse(BMT2)

install.packages("tibble")
bmt2 <- BMT2 |> 
    tibble::rowid_to_column("my_id")
bmt_time <- tmerge(
    data1 = bmt2 |> select(my_id, T1, delta1), 
    data2 = bmt2, 
    id = my_id, 
    death = event(T1, delta1),
    agvhd = tdc(TA)
) |> 
    as_tibble()
bmt_time

head(bmt2, 5)
head(bmt_time, 5)

coxph(
    Surv(time = tstart, time2 = tstop, event = death) ~ agvhd, 
    data = bmt_time
) %>% 
    gtsummary::tbl_regression(exp = TRUE)

