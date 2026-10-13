library(readr)
library(dplyr)
library(jsonlite)
library(stringr)

# =========================
# 1. LECTURE CSV
# =========================

labels <- read_csv("ui_labels.csv", show_col_types = FALSE)

# =========================
# 2. VALIDATION
# =========================

required_cols <- c("variable","fr","en","es")

missing_cols <- setdiff(required_cols, names(labels))

if(length(missing_cols) > 0){
  stop(paste("Missing columns:", paste(missing_cols, collapse=", ")))
}

# =========================
# 3. NETTOYAGE
# =========================

labels <- labels %>%
  mutate(
    variable = str_trim(variable),
    fr = str_trim(fr),
    en = str_trim(en),
    es = str_trim(es)
  )

# enlever lignes sans variable
labels <- labels %>% filter(!is.na(variable) & variable != "")

# =========================
# 4. CONSTRUCTION JSON
# =========================

ui_labels <- labels %>%
  split(.$variable) %>%
  lapply(function(row){
    
    list(
      default = list(
        fr = row$fr[1],
        en = row$en[1],
        es = row$es[1]
      )
    )
    
  })

# =========================
# 5. EXPORT
# =========================

write_json(
  ui_labels,
  "ui_labels.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("ui_labels.json generated successfully\n")