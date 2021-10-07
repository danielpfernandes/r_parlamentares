library(tidyverse)
library(readr)
library(lubridate)

unzip("~/R/parlamentares/parlamentares.zip", exdir = "~/R/parlamentares")

parlamentares <- read_delim(
    "~/R/parlamentares/parlamentares.csv",
    delim = ";",
    escape_double = FALSE,
    locale = locale(
        date_names = "br",
        decimal_mark = ",",
        grouping_mark = "."
    ),
    trim_ws = TRUE
)

names(parlamentares) <- make.names(names(parlamentares))
parlamentares$PERÍODO <- ym(parlamentares$PERÍODO)

lotacao_x_remun <-
    ggplot(data = filter(parlamentares, PERÍODO == "2012-09-01"
                         & REM_LIQUIDA > 0)) +
    geom_point(mapping = aes(x = ANO.EXERCÍCIO,
                             y = REM_LIQUIDA,
                             color = CATEGORIA)) +
    facet_wrap( ~ VÍNCULO, nrow = 2)

cargo_x_remun <-
    ggplot(data = filter(parlamentares, PERÍODO == "2012-09-01"
                         & REM_LIQUIDA > 0)) +
    geom_point(mapping = aes(
        x = CARGO,
        y = REM_LIQUIDA,
        color = ANO.EXERCÍCIO,
        size = REM_LIQUIDA
    )) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

lotacao_x_exercicio <- ggplot(
    data = filter(
        parlamentares,
        PERÍODO == "2021-08-01"
        & ANO.EXERCÍCIO > 1999
        & REM_LIQUIDA > 0
        & VÍNCULO == "COMISSIONADO"
    )
) +
    geom_bar(mapping = aes(x = ANO.EXERCÍCIO)) +
    scale_x_continuous(breaks = seq(1998, 2021, by = 4))

print(cargo_x_remun)
print(lotacao_x_exercicio)
print(lotacao_x_remun)
