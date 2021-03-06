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
parlamentares$PER�ODO <- ym(parlamentares$PER�ODO)

lotacao_x_remun <-
    ggplot(data = filter(parlamentares, PER�ODO == "2012-09-01"
                         & REM_LIQUIDA > 0)) +
    geom_point(mapping = aes(x = ANO.EXERC�CIO,
                             y = REM_LIQUIDA,
                             color = CATEGORIA)) +
    facet_wrap( ~ V�NCULO, nrow = 2)

cargo_x_remun <-
    ggplot(data = filter(parlamentares, PER�ODO == "2012-09-01"
                         & REM_LIQUIDA > 0)) +
    geom_point(mapping = aes(
        x = CARGO,
        y = REM_LIQUIDA,
        color = ANO.EXERC�CIO,
        size = REM_LIQUIDA
    )) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

lotacao_x_exercicio <- ggplot(
    data = filter(
        parlamentares,
        PER�ODO == "2021-08-01"
        & ANO.EXERC�CIO > 1999
        & REM_LIQUIDA > 0
        & V�NCULO == "COMISSIONADO"
    )
) +
    geom_bar(mapping = aes(x = ANO.EXERC�CIO)) +
    scale_x_continuous(breaks = seq(1998, 2021, by = 4))

print(cargo_x_remun)
print(lotacao_x_exercicio)
print(lotacao_x_remun)
