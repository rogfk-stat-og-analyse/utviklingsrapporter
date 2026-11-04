# Laster inn pakker
if (!require(pacman)) install.packages("pacman")
p_load(
  future,
  pdftools
)

# Utvalg
rapport_kommuner <- c(
  "Eigersund", "Stavanger", "Haugesund", "Sandnes", "Sokndal",
  "Lund", "Bjerkreim", "Hå", "Klepp", "Time", "Gjesdal", "Sola",
  "Randaberg", "Strand", "Hjelmeland", "Suldal", "Sauda", "Kvitsøy",
  "Bokn", "Tysvær", "Karmøy", "Utsira", "Vindafjord"
)

# Setter working directory
base_build <- file.path(
  getwd(),
  "build"
)

# Fordeler tilgjengelige CPU kjerner -1
plan(
  multisession,
  workers = availableCores() - 1
)

# ------------------
# --- Funksjonen ---
# ------------------
render_kommune <- function(k) {
  # Definerer outdir
  outdir <- file.path(base_build, k)

  # Lager en liten footer.tex for å klemme inn kommunenavn på hver side
  writeLines(
    paste0("\\def\\kommunenavn{", k, "}"),
    file.path(outdir, "footer.tex")
  )

  # Kopierer filer
  file.copy(
    "scripts/utviklingsanalyse.qmd",
    file.path(outdir, "report.qmd"),
    overwrite = TRUE
  )
  file.copy(
    "assets/Næringsliv_Dis_RGB@2x.png",
    file.path(outdir, "Næringsliv_Dis_RGB@2x.png"),
    overwrite = TRUE
  )

  # Endrer wd
  build_wd <- setwd(outdir)

  # This is where the magic happens
  output <- system2(
    "quarto",
    args = c(
      "render",
      "report.qmd",
      "-P",
      paste0("kommunenavn=", k),
      "--output",
      "report.pdf"
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  # Diagnostikk/plukker opp feilkoder
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop(paste(output, collapse = "\n"))
  }

  report_pdf <- file.path(
    outdir,
    "report.pdf"
  )
  if (!file.exists(report_pdf)) {
    stop("Report PDF was not created")
  }

  # Kombinerer title og report for endelig pdf
  title_pdf <- file.path(outdir, "title.pdf")
  final_pdf <- file.path(
    outdir,
    paste0(k, ".pdf")
  )
  pdf_combine(
    input = c(
      title_pdf,
      report_pdf
    ),
    output = final_pdf
  )

  # Clean-up
  # --------
  # Her spyttes figurene ut av quarto
  source_figs <- file.path(outdir, "report_files", "figure-beamer")

  # Her skal figurene ende opp
  figs_dir <- file.path(outdir, "figures")
  dir.create(figs_dir, showWarnings = FALSE)

  # Flytter figurene
  pdf_figs <- list.files(
    source_figs,
    pattern = "\\.pdf$",
    full.names = TRUE
  )
  file.rename(
    pdf_figs,
    file.path(figs_dir, basename(pdf_figs))
  )

  # Spesifiserer hva som skal slettes/beholdes
  all_items <- list.files(outdir, full.names = TRUE)
  items_to_keep <- c(final_pdf, figs_dir)
  items_to_delete <- setdiff(all_items, items_to_keep)
  # Sletter
  unlink(items_to_delete, recursive = TRUE)

  # Skifter wd midlertidig
  out_wd <- setwd(figs_dir)
  # Konverterer figurene til png (beholder pdf)
  for (pdf in list.files(figs_dir, full.names = TRUE)) {
    pdf_convert(pdf, format = "png")
  }
  setwd(out_wd)

  # Pakk hele kommunemappen til zip
  # Bytt til outdir slik zip/7z ikke pakker inn overliggende mappe

  zip_path <- file.path(base_build, paste0(k, ".zip"))
  system2("zip", args = c("-r9X", zip_path, "."))
  # zip(zip_path, files = list.files(recursive = TRUE), flags = "-r9X")

  setwd(build_wd)
  # Slett mappen, behold kun zip-filen
  unlink(outdir, recursive = TRUE)

  # Tommel opp om alt har gått bra
  TRUE
}

# ----------------
# --- Utføring ---
# ----------------
# Kjører datahenting (betinget)
cat("Vil du hente inn data på nytt?\n  1) Ja\n  2) Nei\n")
# I think I need to setwd to scripts here, or change it to run relative to the
# wrapp.r
if (readLines("stdin", n = 1) == "1") source("scripts/data_hente.r")

# Sjekker build mapper mangler (sekvensielt)
mangler_build <-
  rapport_kommuner[!dir.exists(file.path(base_build, rapport_kommuner))]

# Oppretter evt mapper (sekvensielt)
for (k in mangler_build) {
  dir.create(file.path(base_build, k), recursive = TRUE)
}

# Sjekker om title.pdf mangler (sekvensielt)
title_fil <- file.path(base_build, rapport_kommuner, "title.pdf")
mangler_tittel <- rapport_kommuner[!file.exists(title_fil)]

# Generer manglende titler parallelt
if (length(mangler_tittel) > 0) {
  title_futures <- lapply(mangler_tittel, function(k) {
    future(
      {
        outdir <- file.path(base_build, k)
        source("scripts/title.r", local = TRUE)
      },
      seed = TRUE
    )
  })
  invisible(lapply(title_futures, value))
}

# Kjører funksjonen parallelt
futures <- lapply(rapport_kommuner, function(k) {
  future(
    {
      render_kommune(k)
    },
    seed = TRUE
  )
})

# Sjekker resultater fortløpende og avslutter ved første feil
lapply(seq_along(futures), function(i) {
  value(futures[[i]])
  cat(" ferdig: ", rapport_kommuner[i], "\n", sep = "")
})
