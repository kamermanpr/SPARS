############################################################
#                                                          #
#                    Clean SPARS B data                    #
#                                                          #
############################################################

# Load packages
library(magrittr)
library(tidyverse)

# Import data and add column
## Get a list of files in 'orignal-data' directory
file_names <- list.files(path = "data/",
                         pattern = "*.txt")

## Generate a list of imported dataframes
files <- map(.x = paste0("data/", file_names),
             .f = ~ read_tsv(file = .x))

# Name list items
names(files) <- c("ID01", "ID02", "ID03", "ID04",
                  "ID05", "ID06", "ID07")

# Format all names to lowercase
files %<>%
    map(.x = .,
        ~ rename_all(.tbl = .x,
                     funs(str_to_lower)))

# Rename select columns
files %<>%
    map(.x = .,
        ~ rename(.data = .x,
                 PID = partic,
                 block_number = `block number`,
                 trial_number = `trial number`))

# Remove superfluous columns
files %<>%
    map(.x = .,
        ~ select(.data = .x,
                 -version, -timestamp, -`lost msec`, -`free vram`,
                 -`trial contents`, -(starts_with("int_")),
                 -(starts_with("rt_")), -(starts_with("eval_")),
                 -block, -ttl_fired, -iti, -x40))

# Check for missing data in selected columns
## Create column index
cols <- c('PID', 'block_number', 'trial_number', 'intensity', 'scale')

## Print missing cases
map(.x = files,
    ~ filter(.x[cols], !complete.cases(.x[cols])))

## Remove missing data
files %<>%
    map(.x = .,
        ~ filter(.x, complete.cases(.x[cols])))

# Correct known issue with np_rt (reaction time) and
# rating_given_np (stimulus intensity rating) for ID01.
# (Data entered in wrong columns or not at all)
files$ID01 %<>%
    mutate(rating_given_np = case_when(
        scale == 'NRS_NP' ~ NA,
        TRUE ~ NA
    ))

# Join the seven datasets into one dataframe
data <- map_df(.x = files,
               ~ as_data_frame(.x))

# Generate new columns
## Consolidate raw rating scale values into a single column
data %<>%
    mutate(rating = case_when(
        !is.na(rating_given_spars) & scale == 'SPARS' ~ rating_given_spars,
        !is.na(rating_given_p) & scale == 'CNRS_P' ~ rating_given_p,
        !is.na(rating_given_np) & scale == 'NRS_NP' ~ rating_given_np
    )) %>%
    # Drop old columns
    select(-(starts_with('rating_given')))

# Make intensity class character
data %<>%
    mutate(intensity_char = sprintf('%.2f', intensity))

# Recalibrate intensities to a relative, rather than absolute scale
data %<>%
    group_by(PID) %>%
    arrange(intensity) %>%
    mutate(intensity_rank = dense_rank(intensity)) %>%
    ungroup()

# Converted SPARS to a 0-100 positive scale
data %<>%
    mutate(rating_positive = rating + 50)

# Convert NRS_NP and CNRS_P to SPARS-equivalent ranges (+50 to 50)
data %<>%
    # Fix column format to do the maths
    mutate(rating = as.numeric(rating)) %>%
    # CNRS_P conversion(divide rating by 2)
    # NRS_NP conversion (-100 and divide then by 2)
    mutate(rating_equivalent = case_when(
        scale == 'CNRS_P' ~ rating / 2,
        scale == 'NRS_NP' ~ (rating - 100) / 2,
        TRUE ~ rating
    ))

# Select columns
## Select required columns
data %<>%
    select(PID, scale, block_number, trial_number, intensity, intensity_char,
           intensity_rank, rating, rating_positive, rating_equivalent)

# Final tweaks
data %<>%
    # Remove any grouping
    ungroup() %>%
    # Arrange by PID
    arrange(PID)

# Save outputs
write_rds(x = data,
          path = 'data-cleaned/SPARS_B.rds')

write_csv(x = data,
          path = 'data-cleaned/SPARS_B.csv')
