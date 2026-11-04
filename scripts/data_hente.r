# Laste inn nødvendige pakker, funksjoner og scripts
if (!require(pacman)) install.packages("") # checks if pacman is installed
require(pacman) # loads pacman, then pacman loads packages
p_load(
  dplyr,
  httr2,
  openxlsx,
  purrr,
  readr,
  rjstat,
  stringr
)

get_jsonstat <- function(x) {
  request(x) |>
    req_perform() |>
    resp_body_string() |>
    fromJSONstat()
}

source("scripts/kommunesamle.r")

# Henter kommuner fra kommunesamle.r
kommuner <- delreg_lookup

# {{{ Liste over datasettene jeg vil ha
dfs <- c(
  "befolkning",
  "befolkningsendring",
  "innvandrere",
  "utdanningsnivå",
  "fagfelt",
  "uføre",
  "sysslere",
  "sysslere_næring",
  "sysselsetting",
  "lønnstakere_næring",
  "pendling_inn",
  "pendling_ut",
  "pendling",
  "foretak",
  "boliger",
  "hytter",
  "neets",
  "flytting"
)
# }}}

# {{{ Liste over urler
urls <- c(
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/07459/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(unique(kommuner$k_kode), collapse = ","),
    "&codelist[Region]=agg_KommSummer&valuecodes[Alder]=*&codelist[Alder]=vs_AlleAldre00B&valuecodes[Kjonn]=*&heading=ContentsCode,Tid&stub=Region,Kjonn,Alder"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/01222/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(unique(kommuner$k_kode), collapse = ","),
    "&codelist[Region]=agg_KommSummer&valuecodes[ContentsCode]=*&heading=Tid,ContentsCode&stub=Region"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/09817/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(kommuner$kode, collapse = ","),
    "&codelist[Region]=vs_Kommune&valuecodes[Landbakgrunn]=VES,IVE&valuecodes[ContentsCode]=Personer1&valuecodes[InnvandrKat]=B-C&heading=Tid,ContentsCode,Landbakgrunn&stub=Region,InnvandrKat"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/09429/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=PersonerProsent&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(kommuner$kode, collapse = ","),
    "&codelist[Region]=vs_Kommune&valuecodes[Kjonn]=*&valuecodes[Nivaa]=01,02a,11,03a,04a,09a&heading=ContentsCode,Tid&stub=Region,Nivaa,Kjonn"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/11615/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(unique(kommuner$k_kode), collapse = ","),
    "&codelist[Region]=agg_KommSummer&valuecodes[UtdNivaa]=*&valuecodes[Fagfelt]=*&heading=ContentsCode,Tid&stub=Region,UtdNivaa,Fagfelt"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/11715/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=UforetygdPers&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(kommuner$kode, collapse = ","),
    "&codelist[Region]=vs_Kommune&valuecodes[Alder]=18-24,25-34,35-44,45-54,55-61,62-67&heading=ContentsCode,Tid,Alder&stub=Region"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/07984/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=Sysselsatte&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(kommuner$kode, collapse = ","),
    "&codelist[Region]=vs_KommunerS&valuecodes[Alder]=15-19,20-24,25-39,40-54,55-66,67-74&heading=ContentsCode,Tid,Alder&stub=Region"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/07984/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[Region]=",
    gsub("-", ".", paste0(unique(kommuner$k_kode), collapse = ",")),
    "&codelist[Region]=agg_KommSummerS&valuecodes[Alder]=15-19,20-24,25-39,40-54,55-66,67-74&valuecodes[NACE2007]=01-03,05-09,10-33,35-39,41-43,45-47,49-53,55-56,58-63,64-66,68-75,77-82,84,85,86-88,90-99,00&valuecodes[Kjonn]=2,1&valuecodes[ContentsCode]=SysselsatteArb&heading=ContentsCode,Tid&stub=Region,NACE2007,Alder,Kjonn"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/11607/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=Sysselsatt&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(kommuner$kode, collapse = ","),
    "&codelist[Region]=vs_Kommune&valuecodes[Landbakgrunn]=zzz,tot&valuecodes[Alder]=20-66&heading=ContentsCode,Tid,Alder,Landbakgrunn&stub=Region"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/11657/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(kommuner$kode, collapse = ","),
    "&codelist[Region]=vs_KommunerS&valuecodes[ContentsCode]=Lonsstakere&valuecodes[NACE2007]=01-03,05-43,45-82,84,85,86-88,90-99,00&heading=ContentsCode,Tid&stub=Region,NACE2007"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/03321/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=*&valuecodes[ArbstedKomm]=",
    paste0(kommuner$kode, collapse = ","),
    "&valuecodes[Bokommuen]=*&heading=ContentsCode,Tid&stub=ArbstedKomm,Bokommuen"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/03321/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Bokommuen]=",
    paste0(kommuner$kode, collapse = ","),
    "&valuecodes[ArbstedKomm]=*&valuecodes[Tid]=*&heading=ContentsCode,Tid&stub=ArbstedKomm,Bokommuen"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/11616/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(kommuner$kode, collapse = ","),
    "&codelist[Region]=vs_KommuneArbMark&heading=Tid,ContentsCode&stub=Region"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/08076/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(unique(kommuner$k_kode), collapse = ","),
    "&codelist[Region]=agg_KommSummer&valuecodes[NACE2007]=01,02,03,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,41,42,43,45,46,47,49,50,51,52,53,55,56,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,77,78,79,80,81,82,84,85,86,87,88,90,91,92,93,94,95,96,97,99,00&heading=ContentsCode,Tid&stub=Region,NACE2007"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/06265/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(unique(kommuner$k_kode), collapse = ","),
    "&codelist[Region]=agg_KommSummer&valuecodes[BygnType]=*&heading=ContentsCode,Tid,BygnType&stub=Region"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/03174/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(unique(kommuner$k_kode), collapse = ","),
    "&codelist[Region]=agg_KommSummer&valuecodes[BygnType]=*&heading=ContentsCode,Tid&stub=Region,BygnType"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/13563/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(unique(kommuner$k_kode), collapse = ","),
    "&codelist[Region]=agg_KommSummer&valuecodes[HovArbStyrkStatus]=NEET2,A.01xU.01xU.03&codelist[HovArbStyrkStatus]=vs_ArbStatus2018niva4a&valuecodes[InnvandrKat]=A_C-G,B&valuecodes[Alder]=15-74&valuecodes[ContentsCode]=*&heading=ContentsCode,Tid&stub=Region,HovArbStyrkStatus,InnvandrKat,Alder"
  ),
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/13255/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=*&valuecodes[Region]=",
    paste0(kommuner$kode, collapse = ","),
    "&codelist[Region]=vs_Kommune&valuecodes[Alder]=18-29,30%2B&heading=ContentsCode,Tid,Alder&stub=Region"
  )
)
# }}}

# Making the calls
dfs_list <- lapply(urls, get_jsonstat)
names(dfs_list) <- dfs

# Over kommunenivå
url <- "https://data.ssb.no/api/pxwebapi/v2/tables/09429/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[Region]=11&codelist[Region]=vs_FylkerAlle&valuecodes[Nivaa]=01,02a,11,03a,04a,09a&valuecodes[ContentsCode]=PersonerProsent&heading=ContentsCode,Tid&stub=Region,Nivaa"

utdanning_rogaland <- get_jsonstat(url) |>
  mutate(
    år = as.integer(år),
    region = "Rogaland"
  ) |>
  subset(år == max(år))

url <- "https://data.ssb.no/api/pxwebapi/v2/tables/09429/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&codelist[Region]=vs_Landet&valuecodes[Nivaa]=01,02a,11,03a,04a,09a&valuecodes[ContentsCode]=PersonerProsent&heading=ContentsCode,Tid&stub=Nivaa"

utdanning_norge <- get_jsonstat(url) |>
  mutate(
    år = as.integer(år),
    region = "Norge"
  ) |>
  subset(år == max(år))

# Lagrer data til fil
save(
  dfs_list,
  utdanning_norge,
  utdanning_rogaland,
  file = "dfs_list.rds"
)

# {{{ Bedriftsdata
# hovedenheter <- readRDS(
#  "../Regional_utviklingsrapporter/data/rapport_data/Hovedenheter_Rogaland_260623.rds"
# ) |>
#  mutate(
#    virksomhet.orgnr = as.integer(virksomhet.orgnr)
#  )
#
# enhetsreg <- read.table(
#  file = "../Regional_utviklingsrapporter/data/rapport_data/Enhetsreg geokodet versjon 251126.csv",
#  sep = ";",
#  header = TRUE
# ) |>
#  mutate(
#    komnr = as.integer(komnr)
#  )
#
# nacekoder <-
#  read.xlsx(
#    "../Regional_utviklingsrapporter/data/rapport_data/nace_koder.xlsx"
#  ) |>
#  subset(level < 3) |>
#  subset(select = c(code, parentCode, shortName, level))
#
# hovedkoder <- nacekoder |>
#  subset(level == 1) |>
#  subset(select = c(code, shortName))
#
# bedrifter <- left_join(
#  hovedenheter,
#  enhetsreg,
#  by = c("virksomhet.orgnr" = "overordnet.enh")
# ) |>
#  subset(komnr %in% kommuner$kode) |>
#  mutate(
#    nace_agg = str_split_i(nace, pattern = ",", i = 1),
#    nace_agg = if_else(nace_agg == "", "Uoppgitt", nace_agg),
#    nace_agg = if_else(nchar(nace_agg) < 2, paste0("0", nace_agg), nace_agg),
#  ) |>
#  subset(select = c(driftsresultat, nace_agg, komnr, komnavn)) |>
#  left_join(nacekoder, by = c("nace_agg" = "code")) |>
#  left_join(hovedkoder, by = c("parentCode" = "code")) |>
#  subset(parentCode != "P") |>
#  mutate(
#    shortName.y = case_when(
#      shortName.y == "Kulturell virks., idrett, fritidsaktiviteter" ~
#        "Kultur, idrett og fritidsaktiviteter",
#      .default = shortName.y
#    )
#  ) |>
#  summarise(
#    andel = mean(driftsresultat >= 0, na.rm = TRUE),
#    .by = shortName.y
#  )
#
# saveRDS(bedrifter, "bedrifter.rds")

if (file.exists("data/bedrifter.rds")) {
  cat(
    "bedrifter.rds finnes allerede. Sist oppdatert: ",
    format(
      file.info("data/bedrifter.rds")$mtime,
      "%d.%m.%Y %H:%M"
    ),
    "\nRegenerere? [y/N]: ",
    sep = ""
  )

  oppdater <- tolower(
    readLines("stdin", n = 1)
  ) %in% c("y", "yes", "j", "ja")
} else {
  oppdater <- TRUE
}

if (oppdater) {
  hovedenheter <- readRDS(
    paste0(
      "../Regional_utviklingsrapporter/data/rapport_data/",
      "Hovedenheter_Rogaland_260623.rds"
    )
  ) |>
    mutate(
      virksomhet.orgnr = as.integer(virksomhet.orgnr)
    )

  enhetsreg <- read.table(
    file = paste0(
      "../Regional_utviklingsrapporter/data/rapport_data/",
      "Enhetsreg geokodet versjon 251126.csv"
    ),
    sep = ";",
    header = TRUE
  ) |>
    mutate(
      komnr = as.integer(komnr)
    )

  nacekoder <- read.xlsx(
    paste0(
      "../Regional_utviklingsrapporter/data/rapport_data/",
      "nace_koder.xlsx"
    )
  ) |>
    subset(level < 3) |>
    subset(
      select = c(
        code,
        parentCode,
        shortName,
        level
      )
    )

  hovedkoder <- nacekoder |>
    subset(level == 1) |>
    subset(
      select = c(
        code,
        shortName
      )
    )

  bedrifter <- left_join(
    hovedenheter,
    enhetsreg,
    by = c(
      "virksomhet.orgnr" = "overordnet.enh"
    )
  ) |>
    subset(komnr %in% kommuner$kode) |>
    mutate(
      nace_agg = str_split_i(
        nace,
        pattern = ",",
        i = 1
      ),
      nace_agg = if_else(
        nace_agg == "",
        "Uoppgitt",
        nace_agg
      ),
      nace_agg = if_else(
        nchar(nace_agg) < 2,
        paste0("0", nace_agg),
        nace_agg
      )
    ) |>
    subset(
      select = c(
        driftsresultat,
        nace_agg,
        komnr,
        komnavn
      )
    ) |>
    left_join(
      nacekoder,
      by = c("nace_agg" = "code")
    ) |>
    left_join(
      hovedkoder,
      by = c("parentCode" = "code")
    ) |>
    subset(parentCode != "P") |>
    mutate(
      shortName.y = case_when(
        shortName.y ==
          "Kulturell virks., idrett, fritidsaktiviteter" ~
          "Kultur, idrett og fritidsaktiviteter",
        .default = shortName.y
      )
    ) |>
    summarise(
      andel = mean(
        driftsresultat >= 0,
        na.rm = TRUE
      ),
      .by = shortName.y
    )

  saveRDS(
    bedrifter,
    "data/bedrifter.rds"
  )
} else {
  bedrifter <- readRDS("data/bedrifter.rds")
}
# }}}
