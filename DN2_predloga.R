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
  filter(datum_smrti == konec_mandata) %>%
  distinct(predsednik) %>%
  pull(predsednik) %>%
  length()

# ==================================================================
# Končna rešitev
odgovor1a


# ==================================================================
# b)
odgovor1b = podpredsedniki %>%
  filter(!is.na(podpredsednik)) %>%
  distinct(podpredsednik, predsednik) %>%
  group_by(predsednik) %>%
  summarise(stevilo = n()) %>%
  filter(stevilo == 2) %>%
  pull(predsednik) %>%
  length()

# ==================================================================
# Končna rešitev
odgovor1b



# ==================================================================
# c)
odgovor1c = podpredsedniki %>%
  distinct(podpredsednik, predsednik) %>%
  group_by(predsednik) %>%
  summarise(stevilo = sum(!is.na(podpredsednik))) %>%
  filter(stevilo == 0) %>%
  pull(predsednik) %>%
  length()

# ==================================================================
# Končna rešitev
odgovor1c


# ==================================================================
# d)
odgovor1d = podpredsedniki %>%
  filter(!is.na(podpredsednik)) %>%
  distinct(podpredsednik, predsednik) %>%
  group_by(podpredsednik) %>%
  summarise(stevilo = n()) %>%
  filter(stevilo > 1) %>%
  pull(podpredsednik) %>%
  length()

# ==================================================================
# Končna rešitev
odgovor1d


# ==================================================================
# e)
odgovor1e = predsedniki %>%
  filter(!is.na(datum_smrti)) %>%
  distinct(predsednik, datum_smrti) %>%
  group_by(datum_smrti) %>%
  summarise(stevilo = n()) %>%
  left_join(predsedniki, by = "datum_smrti") %>%
  filter(stevilo > 1) %>%
  distinct(datum_smrti, predsednik) %>%
  .[["predsednik"]]


# ==================================================================
# Končna rešitev
odgovor1e


# ==================================================================
# f)

st_prezivelih_naslednikov = function(zap_stevilka, datum_smrti) {
  razlicni_predsedniki = predsedniki %>%
    distinct(zap_stevilka, datum_smrti)
  
  if (is.na(datum_smrti)) {
    sum(
      razlicni_predsedniki$zap_stevilka > zap_stevilka &
        !is.na(razlicni_predsedniki$datum_smrti)
    )
  } else {
    sum(
      razlicni_predsedniki$datum_smrti < datum_smrti &
        razlicni_predsedniki$zap_stevilka > zap_stevilka,
      na.rm = TRUE
    )
  }
}

odgovor1f = predsedniki %>%
  distinct(zap_stevilka, predsednik, datum_smrti) %>%
  mutate(st_naslednikov = mapply(st_prezivelih_naslednikov, zap_stevilka, datum_smrti)) %>%
  filter(st_naslednikov == max(st_naslednikov))

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
           sep = "-") %>%
  mutate(mesec = mesec %>% parse_number()) %>%
  group_by(mesec) %>%
  summarise(stevilo_rojstev = n()) %>%
  filter(stevilo_rojstev == max(stevilo_rojstev)) %>%
  left_join(slovar, by = c("mesec" = "st")) %>%
  pull(ime)


# ==================================================================
# Končna rešitev
odgovor1g



# ==================================================================
# h)
odgovor1h = predsedniki %>%
  distinct(predsednik, datum_smrti) %>%
  drop_na() %>%
  separate(datum_smrti,
           into = c("leto", "mesec", "dan"),
           sep = "-") %>%
  mutate(dan = dan %>% parse_number()) %>%
  mutate(med_10_in_20 = ifelse(10 <= dan &
                                 dan <= 20, TRUE, FALSE)) %>%
  group_by(med_10_in_20) %>%
  summarise(st = n()) %>%
  mutate(delez = (st / sum(st)) * 100) %>%
  filter(med_10_in_20) %>%
  pull(delez)


# ==================================================================
# Končna rešitev
odgovor1h



# ==================================================================
# i)
odgovor1i = predsedniki %>%
  distinct(predsednik) %>%
  mutate(
    ime = word(predsednik, 1),
    priimek = word(predsednik, -1),
    enaka_crka = str_sub(ime, 1, 1) == str_sub(priimek, 1, 1)
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
odgovor2a = predsedniki %>%
  left_join(
    drzave_populacija %>%
      mutate(drzava = drzava %>%
               str_replace_all("District of Columbia", "D.C.")),
    by = c("zvezna_drzava_rojstva" = "drzava"),
    relationship = "many-to-many"
  ) %>%
  rename(
    zvezna_drzava_rojstva_populacija = populacija,
    zvezna_drzava_rojstva_populacija_leto = leto
  ) %>%
  left_join(
    drzave_populacija %>%
      mutate(drzava = drzava %>%
               str_replace_all("District of Columbia", "D.C.")),
    by = c("zvezna_drzava_smrti" = "drzava"),
    relationship = "many-to-many"
  ) %>%
  rename(
    zvezna_drzava_smrti_populacija = populacija,
    zvezna_drzava_smrti_populacija_leto = leto
  ) %>%
  filter(
    zvezna_drzava_smrti_populacija_leto == 2010 &
      zvezna_drzava_rojstva_populacija_leto == 2010
  ) %>%
  distinct(predsednik,
           zvezna_drzava_rojstva_populacija,
           zvezna_drzava_smrti_populacija) %>%
  filter(zvezna_drzava_rojstva_populacija > zvezna_drzava_smrti_populacija) %>%
  pull(predsednik)


# ==================================================================
# Končna rešitev
odgovor2a



# ==================================================================
# b)
odgovor2b = glavna_mesta %>%
  mutate(
    mesto_crke = sapply(str_replace_all(glavno_mesto, " ", ""), nchar),
    drzava_crke = sapply(drzava, nchar)
  ) %>%
  filter(mesto_crke == drzava_crke &
           leto == 2010) %>% # vzamemo le eno leto da se ne ponovijo podatki
  pull(drzava)

# ==================================================================
# Končna rešitev
odgovor2b



# ==================================================================
# c)
odgovor2c = predsedniki %>%
  distinct(predsednik, kraj_rojstva, zvezna_drzava_rojstva) %>%
  left_join(glavna_mesta %>%
              filter(leto == 2010),
            by = c("zvezna_drzava_rojstva" = "drzava")) %>%  # ne potrebujemo večkrat istega podatka
  filter(kraj_rojstva == glavno_mesto) %>%
  pull(predsednik)

odgovor2c_1 = odgovor2c %>%
  length()

odgovor2c_2 = predsedniki %>%
  distinct(predsednik, zvezna_drzava_rojstva) %>%
  left_join(
    drzave_populacija %>%
      group_by(drzava) %>%
      summarise(povprecje = mean(populacija)),
    by = c("zvezna_drzava_rojstva" = "drzava")
  ) %>%
  filter(predsednik %in% odgovor2c) %>% 
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






# ==================================================================
# Končna rešitev
graf1


# ==================================================================
# GRAF2





# ==================================================================
# Končna rešitev
graf2



# ==================================================================
# GRAF3



# ==================================================================
# Končna rešitev
graf3
