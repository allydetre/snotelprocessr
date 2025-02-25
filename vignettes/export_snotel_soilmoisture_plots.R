### Create SWE/soil moisture/derivative plots ###

# *Important to note: this script should be run with processed SNOTEL data
# This is good for exporting a large number of plots!

################################################################################

library(tidyverse)

# define input folder
input_folder <- "path/to/processed/snotel/data/folder"

# define output folder
output_folder <- "path/to/output/folder"

# Ensure the output folder exists
if (!dir.exists(output_folder)) dir.create(output_folder)

# Get a list of all CSV files in the folder
csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

# Loop through each CSV file
for (csv_file in csv_files) {
  # Extract the station name from the file name
  station_name <- tools::file_path_sans_ext(basename(csv_file))
  station_name <- str_remove(station_name, "_processed$") # Remove "_processed" if present
  
  # Read the CSV file
  df <- read_csv(csv_file, show_col_types = FALSE)
  
  # Calculate derivative length 6 - ###***THIS CAN BE CHANGED DEPENDING ON YOUR PREFERRED PARAMS***###
  df <- df %>%
    mutate(
      soilm_p2 = as.numeric(soilm_p2),  # Ensure numeric
      swe = as.numeric(swe),  # Ensure numeric
      deriv_6 = dplyr::lead(soilm_p2, 6) - soilm_p2
    ) %>%
    group_by(year) %>%
    mutate(
      peak_swe = ifelse(all(is.na(swe)), NA, doy[which.max(swe)]),
      before_peak_swe = ifelse(!is.na(peak_swe) & doy >= (peak_swe - 70) & doy < peak_swe, 1, 0),  
      after_peak_swe = ifelse(!is.na(peak_swe) & doy > peak_swe & doy <= (peak_swe + 30), 1, 0)  
    ) %>%
    ungroup()
  
  # Generate the plot
  plot <- ggplot(df) +
    geom_rect(data = df, 
              aes(xmin = pmax(peak_swe - 70, 0),
                  xmax = peak_swe + 30, 
                  ymin = -Inf, ymax = Inf, fill = "SWE Window")) +
    geom_vline(aes(xintercept = peak_swe, color = "Peak Swe"), linetype = "solid", size = 0.75, alpha = 0.65) +
    geom_path(mapping = aes(x = doy, y = soilm_p2, color = "Soil Moisture"), linewidth = 1, alpha = 0.75) +
    geom_path(mapping = aes(x = doy, y = swe/3, color = "SWE"), linewidth = 1, alpha = 0.5) +
    geom_path(mapping = aes(x = doy, y = deriv_6, color = "Derivative Soil Moisture"), linewidth = 1, alpha = 0.5) +
    geom_point(data = df[df$sm_pulse_flag > 0, ], 
               mapping = aes(x = doy, y = soilm_p2, color = "First Pulse Date"), fill = "red", shape = 21, size = 4) +
    scale_y_continuous(name = "% Soil Moisture at 5cm Depth (% total volume)", limits = c(-20, 50), breaks = seq(-20, 50, by = 10), 
                       sec.axis = sec_axis(transform = ~.*3, breaks = seq(-60, 150, by = 30),name = "SWE (cm)")) +
    facet_wrap(~year, scales = "free") +
    scale_x_continuous(limits = c(0, 220), breaks = seq(0, 220, by = 20), expand = expansion(add = c(0, 0))) +
    labs(colour = NULL, title = paste0("Hourly Soil Moisture vs Daily Change in Soil Moisture - ", station_name),
         subtitle = "SM Function - derivative length = 6, multiplier = 0.85", fill = NULL) +
    xlab(NULL) +
    scale_color_manual(values = c("black", "black", "turquoise4", "blue", "turquoise4")) +
    scale_fill_manual(values = c("grey90")) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.box.background = element_rect(color = "black", size = 1),
          axis.text.x = element_text(size = rel(1), color = "black"),
          axis.text.y.left = element_text(size = rel(1.5), color = "blue"),
          axis.text.y.right = element_text(size = rel(1.5), color = "turquoise4"),
          axis.title.y.left = element_text(size = rel(1.5), vjust = 2, color = "blue"),
          axis.title.y.right = element_text(size = rel(1.5), vjust = 2, color = "turquoise4"))
  
  # Save the plot as PNG
  output_file <- file.path(output_folder, paste0(station_name, ".png"))
  ggsave(output_file, plot = plot, width = 15, height = 8, dpi = 300)
  
  cat("Saved plot for", station_name, "to", output_file, "\n")
}