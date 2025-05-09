library(qdap)
library(wordcloud)
library(ggplot2)
library(tm)
library(tidyverse)
library(tidytext)
library(ggthemes)
library(plotly)
library(openxlsx)
#dane <- read.csv("Alaska_oceny.csv") # Przerobione dane z bazy danych w formacie csv
#dane$text[dane$text == " null"] <- NA # Usuwanie null

# FUNKCJE PROGRAMU

# Wczytywanie pliku csv jako ramka danych
# UWAGA: zmienna plik musi być w formacie string
wczytaj_dane <- function(plik){
  dane <- read.csv(plik)
  dane$text[dane$text == " null"] <- NA # Usuwanie null
  return(dane)
}


# Wyodrebnianie opini konkretnych ocen
dane_ocena <- function(dane, ocena){
  wynik <- dane[dane$rating == ocena, ]
  return(wynik)
}


# Tworzenie ramek danych z czestoscia słów w danych ocenach
czestosc_ocena <- function(dane){ 
  wynik <- freq_terms(dane$text, stopwords = Top200Words)
  return(wynik)
}


# Tworzenie wykresów słupkowych z częstością słów
wykres_czestosci <- function(czestosc_slow){
  tytul <- deparse(substitute(czestosc_slow))  
  ggplot(czestosc_slow, aes(x = FREQ, y = reorder(WORD, FREQ))) +
    geom_bar(stat = "identity", fill = "skyblue", color = "darkblue", alpha = 0.8) +
    labs(x = "Częstość", y = "Słowo") +
    ggtitle(tytul) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10), # Dostosowanie rozmiaru czcionki etykiet na osi Y
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Wyśrodkowanie i stylizacja tytułu wykresu
          panel.grid.major.y = element_blank(), # Usunięcie głównych linii siatki poziomej
          panel.grid.minor.y = element_blank(), # Usunięcie mniejszych linii siatki poziomej
          axis.line = element_line(color = "black")) # Dostosowanie linii osi
  
}

wykres_czestosci_interaktywny <- function(czestosc_slow){
  p <- ggplot(czestosc_slow, aes(x = FREQ, y = reorder(WORD, FREQ))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Częstość", y = "Słowo") +
    theme_minimal()
  ggplotly(p)
}


# Tworzenie chmur słów
# Typ: zajrzyj do ?brewer -> rodzaj palety kolorów w chmurze słów - w programie używany Dark2 i Paired
chmura_slow <- function(czestosc_ocen,typ){
  tytul <- deparse(substitute(czestosc_slow))
  wordcloud(czestosc_ocen$WORD, czestosc_ocen$FREQ, colors = brewer.pal(8,typ))
  title(tytul)
}

# Zapisywanie wyników częstości do pliku csv
zapis_csv <- function(){
  for (ocena in 1:5) {
    czestosc <- czestosc_ocena(dane_ocena(dane,ocena))#UWAGA - w wykonywaniu programu nie zmieniać nazwy zmiennej "dane"
    nazwa_pliku <- paste("czestosc_ocena_", ocena, ".csv")
    write.csv(czestosc, file = nazwa_pliku, row.names = FALSE)
  }
}



# WYKONYWANIE PROGRAMU
dane <- wczytaj_dane("Alaska_oceny.csv")
czestosc_ocena_1 <- czestosc_ocena(dane_ocena(dane,1))


chmura_slow(czestosc_ocena_1,"Dark2")
wykres_czestosci(czestosc_ocena_1)
