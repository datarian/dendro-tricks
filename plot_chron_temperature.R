source("to_rwl.R")

# Liest eine Dendroprogramm-Datei ein und speichert sie in tucson-rwl:
rwl <- read.Dendro.toRWL("62167.txt", saveRWL=T)

# Test: Auslesen aus File
rwl <- read.rwl("62167.rwl", format ="tucson")

summary(rwl)

library(dplyr)
library(ggplot2)

# Die Jahre sind die Zeilennamen im eingelesenen File. Für das Zusammenfügen
# mit den Klimadaten müssen sie in eine eigene Spalte:
rwl_to_join <- tibble::rownames_to_column(rwl,var = "Jahr")
colnames(rwl_to_join) <- c("Jahr", "Chrono")

# Die Temperaturabweichungen müssen auf positive Werte tranformiert werden, damit
# sie nachher in log-skala dargestellt werden können. Wir addieren den Absolutwert
# des Minimums zu jeder Abweichung, so dass alle > 0 sind.

joined_data <- dplyr::inner_join(rwl_to_join, meteo, by = "Jahr")

# Plots vorbereiten. Jede geom_*- Zeile fügt eine neue Ebene zum Plot.
# scale_y_log10 bringt die Y-Achse in log-Skala, so dass die unterschiedlichen
# Wertebereiche besser verglichen werden können.
plot_log10 <- ggplot2(data=rwl_to_join, aes(x=Jahr, y=Chrono)) +
    geom_line() +
    geom_line(aes(x=Jahr, y=Abweichung), color="red") +
    scale_y_log10()



# Zweiter Ansatz: Die einzelnen Reihen werden zentriert und reduziert, d.h. der
# Mittelwert wird von jedem Einzelwert subtrahiert, anschliessend wird durch die
# Standardabweichung dividiert. Alle Werte sind dann im Intervall [-1,1]
joined_data_centered <- apply(joined_data[,2:3],2, scale)

plot_scaled <- ggplot2(data=rwl_to_join, aes(x=Jahr, y=Chrono)) +
    geom_line() +
    geom_line(aes(x=Jahr, y=Abweichung), color="red")

# Plots anzeigen:

plot_log10

plot_scaled
