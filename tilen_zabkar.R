####################################################################
#                    2. DOMAČA NALOGA
####################################################################
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)


# uvoz potrebnih tabel
load("tabele_dn2.RData")

####################################################################
#                        ANALIZA PODATKOV
####################################################################

####################################################################
# 1. Naloga
####################################################################

# ==================================================================
# a)
odgovor1a = predsedniki %>%
  filter(datum_smrti == konec_mandata) %>% # zanimajo nas tisti, ki so umrli med mandatom
  distinct(predsednik) %>% # nekateri se pojavijo v vecih vrsticah, vzamemo le enkrat
  pull(predsednik) %>%
  length()

# ==================================================================
# Končna rešitev
odgovor1a


# ==================================================================
# b)

tabela_stevilo_podpredsednikov =  podpredsedniki %>%
  distinct(podpredsednik, predsednik) %>%
  group_by(predsednik) %>%
  summarise(stevilo_podpredsednikov = sum(!is.na(podpredsednik))) # dobimo število podpredsednikov
# !is.na spremeni imena v vrednost 0 ali 1, ko seštejemo dobimo tudi 0 v primeru, ko nimajo podpredsednika
# naredimo tabelo, da lahko uporabimo pri b) in c) nalogi

odgovor1b = tabela_stevilo_podpredsednikov %>%
  filter(stevilo_podpredsednikov == 2) %>%
  pull(predsednik) %>%
  length()

# ==================================================================
# Končna rešitev
odgovor1b



# ==================================================================
# c)
odgovor1c = tabela_stevilo_podpredsednikov %>%
  filter(stevilo_podpredsednikov == 0) %>%
  pull(predsednik) %>%
  length()

# ==================================================================
# Končna rešitev
odgovor1c


# ==================================================================
# d)
odgovor1d = podpredsedniki %>%
  filter(!is.na(podpredsednik)) %>% # odstranimo mandate, kjer ni bilo podpredsednika
  distinct(podpredsednik, predsednik) %>%
  group_by(podpredsednik) %>%
  summarise(stevilo_predsednikov = n()) %>% # dobimo koliko predsednikom so bili podpredsedniki, tokrat ni potrebno istega trika kot pri b), saj je vedno obstajal predsednik
  filter(stevilo_predsednikov > 1) %>%
  pull(podpredsednik) %>%
  length()

# ==================================================================
# Končna rešitev
odgovor1d


# ==================================================================
# e)
odgovor1e = predsedniki %>%
  filter(!is.na(datum_smrti)) %>% # vzamemo predsednike, ki so že umrli
  distinct(predsednik, datum_smrti) %>%
  group_by(datum_smrti) %>%
  summarise(stevilo_mrtvih_na_datum = n()) %>% # dobimo koliko predsednikov je umrlo na datum
  left_join(predsedniki, by = "datum_smrti") %>% # da dobimo imena predsednikov jih združimo nazaj
  filter(stevilo_mrtvih_na_datum > 1) %>%
  distinct(datum_smrti, predsednik) %>% # odstranimo podvojena imena
  .[["predsednik"]] # zanima nas vektor imen


# ==================================================================
# Končna rešitev
odgovor1e


# ==================================================================
# f)

st_prezivelih_naslednikov = function(zap_stevilka, datum_smrti) {
  # funkcija vrne število naslednikov predsednika, ki so umrli pred njim
  razlicni_predsedniki = predsedniki %>%
    distinct(zap_stevilka, datum_smrti) # zanima nas le, da je naslednik (zap_stevilka) in da je umrl prej
  
  if (is.na(datum_smrti)) {
    # če predsednik še ni umrl
    sum(
      razlicni_predsedniki$zap_stevilka > zap_stevilka &
        !is.na(razlicni_predsedniki$datum_smrti)
    ) # preštejemo naslednike, ki so že umrli
  } else {
    sum(
      razlicni_predsedniki$datum_smrti < datum_smrti &
        razlicni_predsedniki$zap_stevilka > zap_stevilka,
      na.rm = TRUE
    ) # preštejemo naslednike, ki so umrli pred datum_smrti
  }
}

odgovor1f = predsedniki %>%
  distinct(zap_stevilka, predsednik, datum_smrti) %>%
  mutate(st_naslednikov = mapply(st_prezivelih_naslednikov, zap_stevilka, datum_smrti)) %>% # ker potrebujemo več argumentov, uporabimo mapply
  filter(st_naslednikov == max(st_naslednikov)) # vzamemo le največje vrednosti

odgovor1f_1 = odgovor1f %>%
  pull(predsednik)

odgovor1f_2 = odgovor1f %>%
  pull(st_naslednikov)

# ==================================================================
# Končna rešitev
odgovor1f_1
odgovor1f_2



# ==================================================================
# g)
slovar =
  tibble(
    ime = c(
      "Januar",
      "Februar",
      "Marec",
      "April",
      "Maj",
      "Junij",
      "Julij",
      "Avgust",
      "September",
      "Oktober",
      "November",
      "December"
    ),
    st = 1:12
  )

odgovor1g = predsedniki %>%
  distinct(predsednik, datum_rojstva) %>%
  separate(datum_rojstva,
           into = c("leto", "mesec", "dan"),
           sep = "-") %>% # zanimajo nas le meseci
  mutate(mesec = parse_number(mesec)) %>%
  group_by(mesec) %>%
  summarise(stevilo_rojstev = n()) %>% # dobimo število rojstev po mesecih
  filter(stevilo_rojstev == max(stevilo_rojstev)) %>% # vzamemo mesece z največ rojstvi
  left_join(slovar, by = c("mesec" = "st")) %>% # preimenujemo iz številk v imena
  pull(ime)


# ==================================================================
# Končna rešitev
odgovor1g



# ==================================================================
# h)
odgovor1h = predsedniki %>%
  distinct(predsednik, datum_smrti) %>%
  drop_na() %>% # racunamo delez le med ze mrtvimi
  separate(datum_smrti,
           into = c("leto", "mesec", "dan"),
           sep = "-") %>% # zanimajo nas dnevi
  mutate(dan = dan %>% parse_number()) %>%
  mutate(med_10_in_20 = ifelse(10 <= dan & dan <= 20, TRUE, FALSE)) %>% # naredimo pomožni stolpec
  group_by(med_10_in_20) %>%
  summarise(stevilo_rojenih = n()) %>%
  mutate(delez = (stevilo_rojenih / sum(stevilo_rojenih)) * 100) %>% # izračunamo delež
  filter(med_10_in_20) %>% # vzamemo delež, ki pripada rojenim med 10. in 20.
  pull(delez)

# ==================================================================
# Končna rešitev
odgovor1h



# ==================================================================
# i)
odgovor1i = predsedniki %>%
  distinct(predsednik) %>%
  mutate( # uporabimo funkcijo word
    ime = word(predsednik, 1), # vrne prvo besedo
    priimek = word(predsednik, -1), # vrne zadnjo besedo
    enaka_crka = str_sub(ime, 1, 1) == str_sub(priimek, 1, 1) # pomožni stolpec
  ) %>%
  filter(enaka_crka) %>%
  pull(predsednik)


# ==================================================================
# i)
odgovor1i




####################################################################
# 2. Naloga
####################################################################

# ==================================================================
# a)

# popravimo tabelo, ker se v tabelah uporabljata drugačni različici D.C.
popravljena_drzave_populacija = drzave_populacija %>%
  mutate(drzava = str_replace_all(drzava, "District of Columbia", "D.C.")) %>% 
  filter(leto == 2010) %>% 
  select(-leto) # ne potrebujemo več leta


odgovor2a = predsedniki %>%
  left_join(popravljena_drzave_populacija, by = c("zvezna_drzava_rojstva" = "drzava")) %>% # dodamo populacije za rojstva
  rename(zvezna_drzava_rojstva_populacija = populacija) %>% # preimenujemo, ker bo imel sicer stolpec s smrtmi enako ime 
  left_join(popravljena_drzave_populacija, by = c("zvezna_drzava_smrti" = "drzava")) %>%
  rename(zvezna_drzava_smrti_populacija = populacija) %>%
  distinct(predsednik, zvezna_drzava_rojstva_populacija, zvezna_drzava_smrti_populacija) %>%
  filter(zvezna_drzava_rojstva_populacija > zvezna_drzava_smrti_populacija) %>%
  pull(predsednik)


# ==================================================================
# Končna rešitev
odgovor2a



# ==================================================================
# b)
odgovor2b = glavna_mesta %>%
  mutate(
    glavno_mesto = str_replace_all(glavno_mesto, " ", ""), # odstranimo presledke, da nchar pravilo deluje
    mesto_crke = sapply(glavno_mesto, nchar), # s funkcijo nchar preštejemo število črk
    drzava_crke = sapply(drzava, nchar)
  ) %>%
  filter(mesto_crke == drzava_crke & leto == 2010) %>% # vzamemo le eno leto da se ne ponovijo podatki
  pull(drzava)

# ==================================================================
# Končna rešitev
odgovor2b



# ==================================================================
# c)
glavna_mesta_2010 = glavna_mesta %>% 
  filter(leto == 2010) # zanimajo nas le glavna_mesta in ne populacija

odgovor2c = predsedniki %>%
  distinct(predsednik, kraj_rojstva, zvezna_drzava_rojstva) %>%
  left_join(glavna_mesta_2010,by = c("zvezna_drzava_rojstva" = "drzava")) %>% 
  filter(kraj_rojstva == glavno_mesto) %>%
  pull(predsednik)

odgovor2c_1 = odgovor2c %>%
  length()

drzave_populacija_povprecje = drzave_populacija %>%
  group_by(drzava) %>%
  summarise(povprecje = mean(populacija)) # prilagodimo tabelo za left_join naprej

odgovor2c_2 = predsedniki %>%
  distinct(predsednik, zvezna_drzava_rojstva) %>%
  left_join(drzave_populacija_povprecje, by = c("zvezna_drzava_rojstva" = "drzava")) %>%
  filter(predsednik %in% odgovor2c) %>% # vzamemo ustrezne predsednike
  filter(povprecje == max(povprecje)) %>%
  pull(predsednik)



# ==================================================================
# Končna rešitev
odgovor2c_1
odgovor2c_2



####################################################################
#                        VIZUALIZACIJA PODATKOV
####################################################################

# ==================================================================
# GRAF1

podatki1 = predsedniki %>%
  distinct(predsednik, leto_volitev, zvezna_drzava_rojstva) %>% # dobimo toliko vrstic, kolikor mandatov je imel
  group_by(predsednik) %>%
  mutate(st_mandatov = n()) %>% # mutate namesto summarise, da obdrzimo se ostale podatke
  ungroup() %>% # ne potrebujemo več skupin po predsednikih
  distinct(predsednik, zvezna_drzava_rojstva, st_mandatov) %>% # potrebno, da v naslednjem group_by ne štejemo preveč
  mutate(st_mandatov = as.factor(st_mandatov)) %>% # da dobimo ločene barve na grafu
  group_by(zvezna_drzava_rojstva) %>%
  mutate(st_predsednikov_iz_zvezne_drzave = n()) %>%
  ungroup()


graf1 = ggplot(podatki1) +
  aes(
    x = reorder(zvezna_drzava_rojstva, st_predsednikov_iz_zvezne_drzave), # uredimo po velikosti
    fill = st_mandatov # barve stolpcev so število mandatov
  ) +
  geom_bar() +
  coord_flip() + # obrnemo osi
  labs(x = "Zvezna država", y ="Število predsednikov") + # dodamo imena osi
  ggtitle("Število predsednikov, rojenih v posamezni državi") + # naslov
  scale_fill_manual(values = c("#e6e6fa", "#ff6eb4", "#8b3a62"),
                    name = "Število\nmandatov") + # ročno popravimo barve, najdene s color picker
  theme(
    # ročno popravimo, da se ujema z danim grafom
    axis.title = element_text(color = "#8b3a62", face = "bold", size = 13),
    legend.title = element_text(color = "black", face = "bold"),
    plot.title = element_text(
      color = "black",
      face = "bold",
      hjust = 0.5,
      size = 16
    ),
    axis.text.x = element_text(color = "#8b3a62", face = "bold"),
    panel.grid.major = element_line(color = "cornsilk2"),
    panel.grid.minor = element_line(color = "cornsilk2"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.background = element_rect(fill="white")
  )



# ==================================================================
# Končna rešitev
graf1


# ==================================================================
# GRAF2
izracunaj_starost = function(datum_rojstva, trenutni_datum) {
  # funkcija, ki izračuna starost v letih
  datum_rojstva = as.Date(datum_rojstva)
  trenutni_datum = as.Date(trenutni_datum)
  as.integer((trenutni_datum - datum_rojstva) / 365.25)
}

uvrsti_v_obdobje = function(leto) {
  # funkcija, ki uvrsti leto v obdobje
  case_when(
    1750 <= leto & leto <= 1799 ~ "1750-1799",
    1800 <= leto & leto <= 1849 ~ "1800-1849",
    1850 <= leto & leto <= 1899 ~ "1850-1899",
    1900 <= leto & leto <= 1949 ~ "1900-1949",
    1950 <= leto & leto <= 1999 ~ "1950-1999",
    2000 <= leto & leto <= 2021 ~ "2000-2021",
    TRUE ~ NA
  )
}

podatki2 = predsedniki %>%
  mutate(leto_mesec_dan_zacetka_mandata = zacetek_mandata) %>% # kopiramo stolpec, ker zacetek_mandata še potrebujemo
  separate(
    leto_mesec_dan_zacetka_mandata,
    into = c("zacetek_mandata_leto", "zacetek_mandata_mesec", "zacetek_mandata_dan"),
    sep = "-"
  ) %>%
  mutate(
    obdobje_mandata = sapply(zacetek_mandata_leto, uvrsti_v_obdobje),
    starost_zacetek_mandata = mapply(izracunaj_starost, datum_rojstva, zacetek_mandata),
    starost_konec_mandata = mapply(izracunaj_starost, datum_rojstva, konec_mandata)
  ) %>%
  distinct(predsednik,
           starost_zacetek_mandata,
           starost_konec_mandata,
           obdobje_mandata) %>% # te stvari nas zanimajo, predsednik je indeks
  group_by(obdobje_mandata) %>%
  summarise(
    povp_starost_zacetek = mean(starost_zacetek_mandata),
    povp_starost_konec = mean(starost_konec_mandata, na.rm = TRUE), # računamo povprečje med mrtvimi
    st_predsednikov = n()
  ) %>% 
  mutate(sredina = (povp_starost_zacetek + povp_starost_konec) / 2) %>% # dodamo sredino, da lahko tja postavimo število predsednikov na grafu
  pivot_longer(cols = c(povp_starost_zacetek, povp_starost_konec),
               names_to = "mandat",
               values_to = "povp_starost") %>% # da lahko enostavno narišemo oba grafa hkrati
  mutate(mandat = factor(mandat, levels = c("povp_starost_zacetek", "povp_starost_konec"))) # da je tabela pravilno urejena

graf2 = ggplot(podatki2) +
  aes(x = obdobje_mandata, y = povp_starost, color = mandat, group = mandat) + # pogrupiramo po mandatu
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_label(
    # dodamo število predsednikov v kvadratu vmes in jih oblikujemo
    aes(y = sredina, # tukaj uporabimo shranjeno sredino za pozicijo
      label = st_predsednikov),
    color = "#ca77f3",
    fill = "white",
    label.size = 0.3,
    label.padding = unit(0.2, "lines"),
    label.r = unit(0.15, "lines")
  ) +
  labs( # dodamo naslove in ime legendi
    x = "Obdobje",
    y = "Povprečna starost v letih",
    title = "Povprečna starost predsednikov na začetku in koncu mandata",
    subtitle = "V kvadratih je število predsednikov v obdobju",
    color = "Povprečna starost"
  ) +
  scale_color_manual(
    values = c("povp_starost_zacetek" = "#4ad1ff", "povp_starost_konec" = "#4e7ca9"),
    labels = c("Začetek mandata", "Konec mandata")
  ) +
  guides(# odstranimo točke iz legende
    color = guide_legend(override.aes = list(shape = NA))) +
  ylim(50, 70) +
  theme(
    panel.background = element_rect(fill = "white", color = NA), # odstranimo ozadja
    axis.line = element_line(color = "black", linewidth = 0.8), # naredimo debelejše osi
    axis.ticks = element_line(color = "black", linewidth = 0.8), # debelejše črtice
    panel.grid.major = element_blank(), # odstranimo mrežo
    panel.grid.minor = element_blank(),
    plot.title = element_text( # naslov
      color = "black",
      face = "bold",
      hjust = 0.5,
      size = 14
    ),
    plot.subtitle = element_text(color = "black", hjust = 0.5), # podnaslov
    axis.title = element_text( # naslova osi
      color = "#104e8b",
      face = "italic",
      size = 13
    )
  )

# ==================================================================
# Končna rešitev
graf2



# ==================================================================
# GRAF3

# potreben paket rnaturalearthhires, da lahko dobimo meje med zveznimi državami
library(rnaturalearthhires)
library(sf)

rojstva_drzave = predsedniki %>%
  distinct(predsednik, zvezna_drzava_rojstva) %>%
  group_by(zvezna_drzava_rojstva) %>%
  summarise(stevilo_rojstev = n())

smrti_drzave = predsedniki %>%
  distinct(predsednik, zvezna_drzava_smrti) %>%
  mutate(zvezna_drzava_smrti = zvezna_drzava_smrti %>%
           str_replace_all("D.C.", "District of Columbia")) %>% # popravimo, da se ujemajo podatki
  group_by(zvezna_drzava_smrti) %>%
  summarise(stevilo_smrti = n()) %>%
  drop_na()

zda = ne_states(country = "united states of america", returnclass = "sf") # dobimo zemljevid zda

podatki3 = drzave_populacija %>%
  distinct(drzava) %>%
  left_join(rojstva_drzave, by = c("drzava" = "zvezna_drzava_rojstva")) %>%
  left_join(smrti_drzave, by = c("drzava" = "zvezna_drzava_smrti")) %>% 
  mutate(
    stevilo_rojstev = ifelse(is.na(stevilo_rojstev), 0, stevilo_rojstev), # vrednosti na pomenijo, da se noben predsednik ni rodil tam
    stevilo_smrti = ifelse(is.na(stevilo_smrti), 0, stevilo_smrti),
  ) %>%
  right_join(zda, by = c("drzava" = "name")) %>% # dodamo podatke k zda, da imamo geometry stolpec
  select(drzava, stevilo_rojstev, stevilo_smrti, geometry) %>%
  pivot_longer( # format za facet_wrap
    cols = c(stevilo_rojstev, stevilo_smrti),
    names_to = "vrsta",
    values_to = "vrednost"
  ) %>%
  mutate(vrednost = factor(vrednost, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))) %>% # da zagotovimo urejenost v končni tabeli
  st_as_sf() # naredimo v sf objekt

graf3 = ggplot(podatki3) +
  aes(geometry = geometry, fill = vrednost) +
  geom_sf() +
  facet_wrap( ~ vrsta, labeller = labeller(
    vrsta = c(stevilo_rojstev = "Zvezne države rojstva", stevilo_smrti = "Zvezne države smrti")
  )) + # da dobimo grafa drug ob drugem, uporabimo labeller da dodamo naslova
  scale_fill_manual( # popravimo barve
    values = c(
      "0" = "#ffffff",
      "1" = "#f0f0f0",
      "2" = "#e1d9f3",
      "3" = "#c7aadf",
      "4" = "#9e73b5",
      "5" = "#7f409e",
      "7" = "#7f7fc9",
      "8" = "#5c4b8c",
      "9" = "#40005c"
    )
  ) +
  labs(title = "Zvezne države ZDA po številu predsednikov", 
       fill = "Število predsednikov", 
       caption = "Vir: Wikipedija") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12, hjust = 0),
    legend.title.position = "top",
    strip.text = element_text(
      size = 10,
      face = "bold",
      color = "black", 
      margin = margin(10, 0, 10, 0) # širši okvir
    ),
    strip.background = element_rect(fill = "#f0ebf4", color = "black"), # pobarvamo ozdaja
    panel.grid = element_blank(), # odstranimo ozadje in ostalo
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, size = 10)
  ) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) # samo celinska zda



# ==================================================================
# Končna rešitev
graf3
