# Simple beginner-friendly script:
# Create a CBC design that NEVER contains the combination
# price == "29.99" AND router_included == "Yes - high-end Wi-Fi 6/7 mesh router free"
#
# Strategy: keep calling create_cbc_design with different seeds
# until a design is produced that satisfies the restriction.
#
# Result Note: Good design found with seed = 235 (attempt 135).

library(data.table)

# 1) define attribute levels (same as your example)
attr_lvls <- list(
  download_speed = c("50 Mbit/s","250 Mbit/s","1.000 Mbit/s"),
  router_included = c("No (you use your own or buy)","Yes - basic router free",
                      "Yes - high-end Wi-Fi 6/7 mesh router free"),
  bundles = c("None",
              "+ Mobile flat (unlimited 5G SIM for smartphone)",
              "Free landline flat (unlimited calls within Germany)"
  ),
  price = c("29.99", "49.99", "69.99")
)

# 2) helper: test whether a design contains the forbidden combo
contains_forbidden <- function(des) {
  d <- as.data.table(des)
  # ensure character comparisons (factors can cause issues)
  if ("router_included" %in% names(d)) d[, router_included := as.character(router_included)]
  if ("price" %in% names(d)) d[, price := as.character(price)]
  any(d$price == "29.99" & d$router_included == "Yes - high-end Wi-Fi 6/7 mesh router free")
}

# 3) try generating designs until one satisfies the restriction
max_tries <- 500          
base_seed <- 100          # starting seed 
found <- FALSE
for (i in seq_len(max_tries)) {
  seed_try <- base_seed + i
  des_try <- create_cbc_design(
    attr_lvls = attr_lvls,
    n_alt = 3,
    n_cs = 16,
    seed = seed_try
  )
  if (!contains_forbidden(des_try)) {
    design <- as.data.table(des_try)   # final design that meets the restriction
    found <- TRUE
    message(sprintf("Good design found with seed = %d (attempt %d).", seed_try, i))
    break
  }
}

if (!found) {
  stop("No valid design found within max_tries. Consider increasing max_tries or relaxing the constraint.")
}

# 4) inspect the first rows
print(head(design))

# 5) (optional) save to CSV for later use
fwrite(design, "design_restricted.csv", quote = TRUE)
message("Wrote design_restricted.csv to working directory.")