library(data.table)

design <- create_cbc_design(
  attr_lvls = list(
    brand = c("O2", "Vodafone", "T-Mobile"),
    download_speed = c("50 Mbit/s","100 Mbit/s","250 Mbit/s","500 Mbit/s","1.000 Mbit/s"),
    contract_length = c("24 months (standard)","12 months","1 month (monthly cancelable)"),
    router_included = c("No (you use your own or buy)","Yes - basic router free",
                        "Yes - high-end Wi-Fi 6/7 mesh router free"),
    bundles = c("None",
                "+ German TV package (100+ channels)",
                "+ Mobile flat (unlimited 5G SIM for smartphone)",
                "Free landline flat (unlimited calls within Germany)",
                "6 months free (50 % discount first half year)"),
    price = c("19.99", "29.99", "39.99", "49.99", "59.99")
  ),
  n_alt = 3,
  n_cs = 16,
  seed = 100
)

head(design)
print(design)

####################################################################
# Attributes are everything except cs and alt
attr_cols <- setdiff(names(design), c("cs", "alt"))

# If any attributes are factors, convert to character to preserve text in Excel
design[, (attr_cols) := lapply(.SD, function(x) {
  if (is.factor(x)) as.character(x) else x
}), .SDcols = attr_cols]

all_alt_rows <- copy(design)[, Option := alt][, c("cs", "Option", attr_cols), with = FALSE]
setorder(all_alt_rows, cs, Option)

# ---- create a list of sheets: All_Alternatives + CS_1 ... CS_N ------------
sheets_list <- list()
sheets_list[["All_Alternatives"]] <- as.data.frame(all_alt_rows)

# create one sheet per cs (only if number of CS not too huge; Excel supports many sheets)
unique_cs <- sort(unique(all_alt_rows$cs))
for (i in unique_cs) {
  sheet_name <- paste0("CS_", i)
  sheets_list[[sheet_name]] <- as.data.frame(all_alt_rows[cs == i])
}

out_csv  <- "design_all_alternatives.csv"

if (!requireNamespace("writexl", quietly = TRUE)) {
  message("Installing package 'writexl'...")
  install.packages("writexl")
}


# CSV (single file) for the combined view
fwrite(all_alt_rows, out_csv, quote = TRUE)

