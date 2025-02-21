# UO Brand Theme for R Visualizations
# Based on UO Visual Identity Guidelines
# https://communications.uoregon.edu/uo-brand/visual-identity/colors

# Required packages
library(ggplot2)
library(tibble)

#===============================
# UO Brand Color Definitions
#===============================
uo_colors <- list(
  # Primary Colors
  green = "#154733",      # UO Green
  yellow = "#FEE123",     # UO Yellow
  
  # Secondary Colors - Greens
  legacy_green = "#004F27", # Legacy Green
  grass_green = "#518241",  # Grass Green
  lime_green = "#7BAE28",   # Lime Green
  chartreuse = "#9FD430",   # Chartreuse
  
  # Secondary Colors - Blues
  dark_blue = "#004D6C",    # Dark Blue
  light_blue = "#3F8EA8",   # Light Blue
  
  # Secondary Colors - Berry
  berry = "#820043",        # Berry
  
  # Secondary Colors - Grays
  dark_gray = "#2E2E2E",    # Dark Gray
  medium_gray = "#7C8487",  # Medium Gray
  light_gray = "#E6E7E8",   # Light Gray
  
  # Accent Colors
  pine = "#004225",         # Pine
  forest = "#0E4B33",       # Forest
  sage = "#899A75",         # Sage
  moss = "#D5DD98",         # Moss
  spring = "#A1D296",       # Spring
  blue = "#84A4CC",         # Blue
  tan = "#B3A369"          # Tan
)

#===============================
# Color Palette Functions
#===============================
# Primary palette (UO Green, Yellow, and grays)
uo_palette_primary <- function(n = 4) {
  colors <- c(uo_colors$green,
              uo_colors$yellow,
              uo_colors$dark_gray,
              uo_colors$medium_gray)
  colors[1:n]
}

# Greens palette
uo_palette_greens <- function(n = 4) {
  colors <- c(uo_colors$legacy_green,
              uo_colors$grass_green,
              uo_colors$lime_green,
              uo_colors$chartreuse)
  colors[1:n]
}

# Cool colors palette (blues and grays)
uo_palette_cool <- function(n = 5) {
  colors <- c(uo_colors$dark_blue,
              uo_colors$light_blue,
              uo_colors$dark_gray,
              uo_colors$medium_gray,
              uo_colors$light_gray)
  colors[1:n]
}

# Warm colors palette - expanded to include 4 colors
uo_palette_warm <- function(n = 4) {
  colors <- c(uo_colors$yellow,
              uo_colors$berry,
              uo_colors$tan,
              uo_colors$grass_green)  # Added a fourth color
  colors[1:n]
}
# Complete palette
uo_palette_all <- function(n = 17) {
  colors <- c(
    # Primary
    uo_colors$green,
    uo_colors$yellow,
    # Secondary Greens
    uo_colors$legacy_green,
    uo_colors$grass_green,
    uo_colors$lime_green,
    uo_colors$chartreuse,
    # Blues
    uo_colors$dark_blue,
    uo_colors$light_blue,
    # Berry
    uo_colors$berry,
    # Grays
    uo_colors$dark_gray,
    uo_colors$medium_gray,
    uo_colors$light_gray,
    # Accent Colors
    uo_colors$pine,
    uo_colors$sage,
    uo_colors$moss,
    uo_colors$spring,
    uo_colors$tan
  )
  colors[1:n]
}

#===============================
# UO Brand Theme Definition
#===============================
theme_uo <- function(base_size = 12, style = "light") {
  # Define background and text colors based on style
  bg_color <- if(style == "dark") uo_colors$dark_gray else "white"
  text_color <- if(style == "dark") "white" else uo_colors$dark_gray
  grid_color <- if(style == "dark") uo_colors$gray else uo_colors$light_gray
  
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Text elements
      text = element_text(
        family = "sans",
        color = text_color
      ),
      plot.title = element_text(
        size = rel(1.4),
        face = "bold",
        color = text_color
      ),
      plot.subtitle = element_text(
        size = rel(1.1),
        color = text_color
      ),
      
      # Axis formatting
      axis.title = element_text(
        face = "bold",
        color = text_color
      ),
      axis.text = element_text(
        color = text_color
      ),
      
      # Panel elements
      panel.background = element_rect(fill = bg_color, color = NA),
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.grid.major = element_line(color = grid_color),
      panel.grid.minor = element_blank(),
      
      # Legend formatting
      legend.background = element_rect(fill = bg_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(face = "bold", color = text_color),
      legend.position = "bottom"
    )
}

#===============================
# Scale Functions
#===============================
scale_fill_uo <- function(palette = "primary", ...) {
  pal <- switch(palette,
                "primary" = uo_palette_primary(),
                "nature" = uo_palette_nature(),
                "all" = uo_palette_all(),
                uo_palette_primary())  # default to primary if invalid choice
  
  scale_fill_manual(values = pal, ...)
}

scale_color_uo <- function(palette = "primary", ...) {
  pal <- switch(palette,
                "primary" = uo_palette_primary(),
                "nature" = uo_palette_nature(),
                "all" = uo_palette_all(),
                uo_palette_primary())  # default to primary if invalid choice
  
  scale_color_manual(values = pal, ...)
}

#===============================
# Example Usage
#===============================
# Example data
example_data <- tibble(
  category = c("Program A", "Program B", "Program C", "Program D"),
  enrollment = c(150, 280, 210, 320),
  department = factor(c("Sciences", "Humanities", "Arts", "Social Sciences"))
)

# Example 1: Using Green Palette
p1 <- ggplot(example_data, 
             aes(x = reorder(category, enrollment), 
                 y = enrollment, 
                 fill = department)) +
  geom_col() +
  scale_fill_manual(values = uo_palette_greens()) +
  theme_uo() +
  labs(
    title = "Program Enrollment",
    subtitle = "Using UO Green Palette",
    x = "Program",
    y = "Number of Students",
    fill = "Department"
  ) +
  coord_flip()

# Example 2: Cool Colors
p2 <- ggplot(example_data, 
             aes(x = category, 
                 y = enrollment, 
                 fill = department)) +
  geom_col() +
  scale_fill_manual(values = uo_palette_cool()) +
  theme_uo() +
  labs(
    title = "Program Enrollment",
    subtitle = "Using UO Cool Colors",
    x = "Program",
    y = "Number of Students",
    fill = "Department"
  )

# Example 3: Warm Colors with Dark Theme
p3 <- ggplot(example_data, 
             aes(x = category, 
                 y = enrollment, 
                 fill = department)) +
  geom_col() +
  scale_fill_manual(values = uo_palette_warm()) +
  theme_uo(style = "dark") +
  labs(
    title = "Program Enrollment",
    subtitle = "Using UO Warm Colors",
    x = "Program",
    y = "Number of Students",
    fill = "Department"
  )

# Example 4: Green Sequential Gradient
example_data_sequential <- tibble(
  category = c("Program A", "Program B", "Program C", "Program D", "Program E"),
  enrollment = c(150, 280, 210, 320, 190)
)

p4 <- ggplot(example_data_sequential, 
             aes(x = category, 
                 y = enrollment,
                 fill = enrollment)) +
  geom_col() +
  scale_fill_gradient(
    low = uo_colors$chartreuse,    # Lighter green
    high = uo_colors$legacy_green, # Darker green
    guide = "colorbar"
  ) +
  theme_uo() +
  labs(
    title = "Program Size",
    subtitle = "Using UO Green Sequential Gradient",
    x = "Program",
    y = "Number of Students"
  )

# Example 5: Diverging Color Scale
example_data_diverging <- tibble(
  department = c("Biology", "Chemistry", "Physics", "Math", "Computer Science"),
  change = c(15, -10, 5, -8, 12),  # Percent change in enrollment
  baseline = 0
)

p5 <- ggplot(example_data_diverging) +
  geom_col(aes(x = department, y = change, fill = change)) +
  geom_hline(yintercept = 0, color = uo_colors$medium_gray) +
  scale_fill_gradient2(
    low = uo_colors$berry,      # Negative values
    mid = uo_colors$light_gray, # Zero point
    high = uo_colors$grass_green, # Positive values
    midpoint = 0,
    guide = "colorbar"
  ) +
  theme_uo() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Enrollment Change by Department",
    subtitle = "Using UO Colors for Diverging Data",
    x = "Department",
    y = "Percent Change in Enrollment",
    fill = "% Change"
  )

# Example 6: Primary Colors Only (UO Green and Yellow)
example_data_primary <- tibble(
  term = c("Fall", "Winter", "Spring"),
  enrollment = c(250, 230, 270),
  type = factor(c("New Students", "Returning Students", "Total"))
)

p6 <- ggplot(example_data_primary, 
             aes(x = term, 
                 y = enrollment)) +
  # Create bars in UO Green
  geom_col(fill = uo_colors$green) +
  # Add yellow highlighting for emphasis
  geom_hline(yintercept = mean(example_data_primary$enrollment), 
             color = uo_colors$yellow, 
             size = 1.5,
             linetype = "dashed") +
  theme_uo() +
  labs(
    title = "Term Enrollment",
    subtitle = "Using Only UO Primary Colors",
    x = "Academic Term",
    y = "Number of Students",
    caption = "Yellow line indicates average enrollment"
  )


# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
#===============================
# Usage Notes
#===============================
# To use this theme in your own visualizations:
# 1. Load required packages (ggplot2, tibble)
# 2. Run all definitions above (colors, theme, and scale functions)
# 3. Apply theme_uo() to your ggplot
# 4. Use scale_fill_uo() or use specific palettes:
#    - uo_palette_primary() - Primary UO colors
#    - uo_palette_greens() - Green variations
#    - uo_palette_cool() - Blues and grays
#    - uo_palette_warm() - Yellow, berry, and warm tones
#    - uo_palette_all() - All brand colors
# 5. Access specific colors using uo_colors$color_name
# 6. Optional: Use style = "dark" in theme_uo() for dark mode
# 7. For gradients:
#    - Use scale_fill_gradient() for sequential data
#    - Use scale_fill_gradient2() for diverging data
#    - Choose appropriate color combinations from uo_colors
#
# Note on UO Gradient:
# The official UO gradient (40% Yellow to 60% Green, bottom-left to top-right)
# is reserved for backgrounds and special applications in marketing materials.
# For data visualizations, use solid colors from the UO palette.