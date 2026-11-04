#' Aggregerer data fra kommuner i Rogaland som ble slått sammen eller endret i
#' 2019/2020. MERK: Sandnes behandles som kontinuerlig til tross for nye
#' kommunegrenser.

delreg_lookup <- data.frame(
  kode = as.integer(c(
    1101, 1102, 1103, 1106, 1108,
    1111, 1112, 1114, 1119,
    1120, 1121, 1122, 1124,
    1127, 1129, 1130, 1133, 1134,
    1135, 1141, 1142, 1144, 1145, 1146,
    1149, 1151, 1154, 1160
  )),
  navn = c(
    "Eigersund", "Sandnes", "Stavanger", "Haugesund", "Sandnes", "Sokndal",
    "Lund", "Bjerkreim", "Hå", "Klepp", "Time", "Gjesdal", "Sola",
    "Randaberg", "Forsand", "Strand", "Hjelmeland", "Suldal", "Sauda",
    "Finnøy", "Rennesøy", "Kvitsøy",
    "Bokn", "Tysvær", "Karmøy", "Utsira", "Vindafjord", "Vindafjord"
  ),
  delregion = c(
    "Dalane", "Nord-Jæren", "Nord-Jæren", "Haugalandet", "Nord-Jæren",
    "Dalane", "Dalane", "Dalane", "Jæren",
    "Jæren", "Jæren", "Jæren", "Nord-Jæren",
    "Nord-Jæren", "", "Ryfylke", "Ryfylke", "Ryfylke",
    "Ryfylke", "", "", "Nord-Jæren", "Haugalandet", "Haugalandet",
    "Haugalandet", "Haugalandet", "Haugalandet", "Haugalandet"
  ),
  stringsAsFactors = FALSE
)

delreg_lookup <- delreg_lookup |>
  mutate(
    k_kode = case_when(
      kode %in% c(1102, 1129) ~ "K-1108",
      kode %in% c(1103, 1141, 1142) ~ "K-1103",
      kode %in% c(1154, 1160) ~ "K-1160",
      .default = paste0("K-", kode)
    )
  )

# kommunesamler <- function(df, region = "region", tid = "år") {
kommunesamler <- function(df, region = "region") {
  # Sjekker om region og tid stemmer
  if (!region %in% names(df)) stop("Fant ingen region-varaibel, vennligst anngi region")
  # if (!tid %in% names(df)) stop("Fant ingen tids-varaibel, vennligst anngi tid")

  # Lagrer input variabel navn for å gi samme output
  region_store <- region
  # tid_store <- tid

  # Standariserer før databehandling
  region <- df[[region]]
  # tid <- as.integer(df[[tid]])

  # Hvilke kommuner/kommunenr skal hvor
  if (is.numeric(region)) {
    region[region %in% c(1102, 1129, 1108)] <- 1108
    region[region %in% c(1103, 1141, 1142)] <- 1103
    region[region == 1154] <- 1160
  } else {
    region[region %in% c(
      "Sandnes", "Forsand (-2019)", "Sandnes (-2019)"
    )] <- "Sandnes"
    region[region %in% c(
      "Stavanger", "Finnøy (-2019)", "Rennesøy (-2019)"
    )] <- "Stavanger"
    region[region == "Vindafjord (1965-2005)"] <- "Vindafjord"
  }

  # Oppdaterer region og tid i dataframen
  df[[region_store]] <- region
  # df[[tid_store]] <- tid

  # Grupperer etter alle variabler unntatt value
  df |>
    group_by(across(-value)) |>
    summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      delregion = if (is.numeric(region)) {
        delreg_lookup$delregion[
          match(.data[[region_store]], delreg_lookup$kode)
        ]
      } else {
        delreg_lookup$delregion[
          match(.data[[region_store]], delreg_lookup$navn)
        ]
      }
    )
}
