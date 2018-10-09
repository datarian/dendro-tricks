source("to_rwl.R")
if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
}
if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}
if(!require(reshape2)){
    install.packages("reshape2")
    library(reshape2)
}

# Liest eine Dendroprogramm-Datei ein und speichert sie in tucson-rwl:
# Die Funktion ist in to_rwl.R, die erste Zeile in diesem Script lädt sie zur Verwendung.
# Filename übergeben, saveRWL -> der Inhalt der Datei kommt in rwl-Format in die Variable rwl, und die Datei wird gleichzeitig für spätere Verwendung in .rwl-Format gespeichert
rwl <- read.Dendro.toRWL("62167.txt", saveRWL=T)

# Test: Auslesen aus File
rwl <- read.rwl("62167.rwl", format ="tucson")

# Kurze Übersicht
summary(rwl)



# Die Jahre sind die Zeilennamen im eingelesenen File. Für das Zusammenfügen
# mit den Klimadaten müssen sie in eine eigene Spalte:
rwl_to_join <- tibble::rownames_to_column(rwl,var = "Jahr")

# Dann die Spalten benennen, Jahr als Zahl formatieren
colnames(rwl_to_join) <- c("Jahr", "Chrono")
rwl_to_join$Jahr <- as.numeric(rwl_to_join$Jahr)

# Temperaturabweichungen einlesen
temp <- read.table("Jahrestemperatur_Abweichung.csv", sep=";")
colnames(temp) <- c("Jahr", "Abweichung")

# Die Daten der Chrono und Temp-Abweichungen werden zusammengefügt und in zwei versch. Variablen gespeichert
joined_data <- joined_data_log10 <- dplyr::inner_join(rwl_to_join, temp, by = "Jahr")

# Die Temperaturabweichungen müssen für log10 auf positive Werte tranformiert werden, damit
# sie nachher in log-skala dargestellt werden können. Wir addieren den Absolutwert
# des Minimums zu jeder Abweichung, so dass alle > 0 sind.
t_add <- abs(min(temp$Abweichung))
joined_data_log10$Abweichung <- joined_data_log10$Abweichung + t_add

# Daten für Plot umbauen
data_plot_log10 <- reshape2::melt(joined_data_log10, id.vars = "Jahr", variable.name="Reihe", value.name="Wert")

# Plots vorbereiten. Jede geom_*- Zeile fügt eine neue Ebene zum Plot.
# scale_y_log10 bringt die Y-Achse in log-Skala, so dass die unterschiedlichen
# Wertebereiche besser verglichen werden können.
plot_log10 <- ggplot(data=data_plot_log10, aes(x=Jahr, y=Wert, color=Reihe)) +
    geom_line() +
    scale_y_log10()



# Zweiter Ansatz: Die einzelnen Reihen werden zentriert und reduziert, d.h. der
# Mittelwert wird von jedem Einzelwert subtrahiert, anschliessend wird durch die
# Standardabweichung dividiert. Alle Werte sind dann im Intervall [-1,1]
joined_data_centered <- cbind(joined_data[1],apply(joined_data[,2-3],2,scale))

data_plot_centered <- reshape2::melt(joined_data_centered, id.vars = "Jahr", variable.name="Reihe", value.name="Wert")

plot_scaled <- ggplot(data=data_plot_centered, aes(x=Jahr, color=Reihe)) +
    geom_line(aes(y=Wert))

# Plots anzeigen:

plot_log10

plot_scaled
