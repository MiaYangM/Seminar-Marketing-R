# Detect and optionally fix duplicate alternatives within the same choice set (cs).
# Usage:
# 1) Load or create 'design' in your session (data.table or data.frame).
# 2) source("detect_and_fix_duplicates.R") or paste into console.
#
# The "fix" step (do_fix <- TRUE) will replace duplicate rows with
# random allowed profiles that are not already present in the same cs.
# You can set a seed for reproducibility.

library(data.table)

# ---- Settings ----
do_fix <- TRUE    # set FALSE to only detect and inspect duplicates
fix_seed <- 2025  # change if you want deterministic replacements

# ---- Safety checks ----
if (!exists("design")) stop("No object named 'design' found. Run or load your design first.")
design <- as.data.table(design)
if (!all(c("cs", "alt") %in% names(design))) stop("design must contain columns 'cs' and 'alt'")

# Attributes to compare (exclude cs and alt and optional text column)
attr_cols <- setdiff(names(design), c("cs", "alt", "text"))

# Ensure attribute columns are character for reliable comparisons
design[, (attr_cols) := lapply(.SD, function(x) if (is.factor(x)) as.character(x) else x), .SDcols = attr_cols]

# ---- 1) Find duplicated combos within each cs ----
# Group by cs + attribute columns and count duplicates
dup_summary <- design[, .N, by = c("cs", attr_cols)][N > 1]

if (nrow(dup_summary) == 0) {
  cat("No duplicates found within any cs. You're good.\n")
} else {
  cat("Found duplicated alternatives within these choice sets (cs):\n")
  print(dup_summary[, c("cs", "N"), with = FALSE])
  # Show detailed rows that are duplicated
  problem_rows <- merge(design, dup_summary, by = c("cs", attr_cols), allow.cartesian = TRUE)
  setorder(problem_rows, cs, alt)
  cat("\nDetailed duplicated rows (showing cs, alt and attributes):\n")
  print(problem_rows[, c("cs", "alt", attr_cols), with = FALSE])
  
  # show which cs to inspect
  bad_cs <- sort(unique(problem_rows$cs))
  cat("\nChoice sets with duplicates:", paste(bad_cs, collapse = ", "), "\n")
  
  # Print full content for each problematic cs for manual inspection
  for (cs_id in bad_cs) {
    cat("\n--- cs =", cs_id, " ---\n")
    print(design[cs == cs_id, .(alt, .SD), .SDcols = attr_cols])
  }
  
  # ---- 2) Optionally fix duplicates by replacing with allowed profiles ----
  if (do_fix) {
    # Build set of all possible profiles to sample replacements from.
    # If you have 'attr_lvls' in your environment (the original attribute levels), use it;
    # otherwise infer levels from the design.
    if (exists("attr_lvls") && is.list(attr_lvls)) {
      all_profiles <- as.data.table(do.call(expand.grid, c(attr_lvls, stringsAsFactors = FALSE)))
    } else {
      lvls <- lapply(attr_cols, function(a) sort(unique(design[[a]])))
      names(lvls) <- attr_cols
      all_profiles <- as.data.table(do.call(expand.grid, c(lvls, stringsAsFactors = FALSE)))
    }
    # ensure character
    all_profiles[, (attr_cols) := lapply(.SD, as.character), .SDcols = attr_cols]
    # create id to compare easily
    all_profiles[, pid := do.call(paste, c(.SD, sep = "|")), .SDcols = attr_cols]
    all_pids <- all_profiles$pid
    
    # helper to get pid for a row in design
    design[, pid := do.call(paste, c(.SD, sep = "|")), .SDcols = attr_cols]
    
    set.seed(fix_seed)
    replacements_made <- 0L
    for (cs_id in bad_cs) {
      # pids already present in this cs
      pids_in_cs <- design[cs == cs_id, pid]
      # find duplicates rows (keep first occurrence, others to replace)
      rows_in_cs <- design[cs == cs_id]
      # find duplicated combinations in this cs by pid
      dup_pids <- rows_in_cs[duplicated(pid), pid]
      if (length(dup_pids) == 0) next
      # For each duplicated pid, find all row indices in design for that pid within cs
      for (dup_pid in dup_pids) {
        rows_idx <- which(design$cs == cs_id & design$pid == dup_pid)
        # keep the first row, replace the rest
        replace_idx <- rows_idx[-1]
        for (r in replace_idx) {
          candidates <- setdiff(all_pids, design[cs == cs_id, pid])
          if (length(candidates) == 0) {
            stop(sprintf("No available replacement for cs=%s (all profiles used). Consider enlarging attr levels or different strategy.", cs_id))
          }
          chosen_pid <- sample(candidates, 1)
          # lookup chosen profile attribute values
          vals <- all_profiles[pid == chosen_pid, ..attr_cols]
          # assign values into design row r
          for (col in attr_cols) set(design, r, col, vals[[col]])
          # update pid
          set(design, r, "pid", chosen_pid)
          replacements_made <- replacements_made + 1L
        }
      }
    }
    cat(sprintf("\nReplacements made: %d\n", replacements_made))
    
    # remove pid helper column
    design[, pid := NULL]
    
    # Re-run duplicate check
    dup_summary2 <- design[, .N, by = c("cs", attr_cols)][N > 1]
    if (nrow(dup_summary2) == 0) {
      cat("Duplicates fixed. No duplicates remain.\n")
    } else {
      cat("Warning: some duplicates remain after fix. See dup_summary2 below:\n")
      print(dup_summary2)
    }
    
    # (optional) save fixed design
    fwrite(design, "design_fixed_duplicates.csv", quote = TRUE)
    cat("Wrote design_fixed_duplicates.csv to working directory.\n")
  } # end do_fix
} # end else duplicates found
#########
#Found duplicated alternatives within these choice sets 2 and 16.
# pruce not numerical values

library(data.table)
design_fixed <- fread("design_fixed_duplicates.csv")   # or load the design object if still in memory
design_fixed[cs %in% c(2,16)]

design_fixed[, price := as.numeric(as.character(price))]
# check for NAs after conversion:
any(is.na(design_fixed$price))

### double check any duplicates remain
attr_cols <- setdiff(names(design_fixed), c("cs","alt","text"))
dup_summary2 <- design_fixed[, .N, by = c("cs", attr_cols)][N > 1]
dup_summary2  