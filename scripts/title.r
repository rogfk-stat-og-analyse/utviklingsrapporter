# DETTE SCRIPTET MÅ KJØRES AV wrapp.r!

library(fs)
library(pdftools)
library(withr)

# Lag kommunevise title.qmd
# -------------------------

title <- readLines(
  "scripts/title.qmd",
  encoding = "UTF-8"
)

title <- gsub(
  'subtitle: "KOMMUNE"',
  paste0('subtitle: "', k, '"'),
  title,
  fixed = TRUE
)

title_qmd <- file.path(
  outdir,
  "title.qmd"
)

writeLines(
  title,
  title_qmd,
  useBytes = TRUE
)

# Flytter tillegsfiler
file_copy(
  "assets/Folkehelse_Morgen_RGB@2x.png",
  file.path(
    outdir,
    "Folkehelse_Morgen_RGB@2x.png"
  ),
  overwrite = TRUE
)

file_copy(
  "assets/logo.png",
  file.path(outdir, "logo.png"),
  overwrite = TRUE
)

# ------------------------------------------------------------
# Render title.qmd
# ------------------------------------------------------------

title_full <- file.path(
  outdir,
  "title_full.pdf"
)

with_dir(
  outdir,
  system2(
    "quarto",
    args = c(
      "render",
      "title.qmd",
      "--output",
      "title_full.pdf"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
)

# Sjekker om de faktisk ble laget
if (!file_exists(title_full)) {
  stop(
    "Quarto did not create: ",
    title_full,
    "\n",
    "Check the Quarto output above."
  )
}

title_pdf <- file.path(
  outdir,
  "title.pdf"
)

pdf_subset(
  title_full,
  pages = 1,
  output = title_pdf
)

message(
  "title.pdf laget for ",
  k
)
