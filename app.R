library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(plotly)

#rsconnect::writeManifest()

# ------------------------------------------------------------------
# Data
# ------------------------------------------------------------------
# Adjust this path if your file lives somewhere else
data_file <- "data/Yvoz et al 2021 appendices (1).xlsx"

d <- read_excel(data_file, sheet = "Plant level")

# Lookup tables (full names for species / crops / locations) from the Legend sheet
legend_raw <- read_excel(data_file, sheet = "Legend", col_names = FALSE, skip = 2)

species_lu <- legend_raw |>
  select(Species = 1, Latin = 2) |>
  filter(!is.na(Species), !is.na(Latin)) |>
  distinct(Species, .keep_all = TRUE)

crop_lu <- legend_raw |>
  select(Crop = 4, Crop_name = 5) |>
  filter(!is.na(Crop), !is.na(Crop_name)) |>
  distinct(Crop, .keep_all = TRUE)

loc_lu <- legend_raw |>
  select(Location = 7, Location_name = 8) |>
  filter(!is.na(Location), !is.na(Location_name)) |>
  distinct(Location, .keep_all = TRUE)

# Order of the 'name' variables, and their colours
# name_levels <- c("Pol1", "Pol2", "Pol3",
#                  "PCont1", "PCont2", "PCont3",
#                  "Harm1", "Harm2", "Harm3")

name_levels <- c("HoneyBees", "BumbleBees", "HoverFlies",
                 "FarmlandBirds", "Carabids", "ParasitoidWasps",
                 "CropCoop", "HarvestCoop", "LongtermCoop")

# name_colors <- c(
#   Pol1   = "#FFF176", Pol2   = "#FDD835", Pol3   = "#F9A825",  # yellows
#   PCont1 = "#A5D6A7", PCont2 = "#43A047", PCont3 = "#1B5E20",  # greens
#   Harm1  = "#EF9A9A", Harm2  = "#E53935", Harm3  = "#B71C1C"   # reds
# )

name_colors <- c(
  HoneyBees   = "#FFF176", BumbleBees   = "#FDD835", HoverFlies   = "#F9A825",  # yellows
  FarmlandBirds = "#A5D6A7", Carabids = "#43A047", ParasitoidWasps = "#1B5E20",  # greens
  CropCoop  = "#EF9A9A", HarvestCoop  = "#E53935", LongtermCoop  = "#B71C1C"   # reds
)

# Same wrangling as in starburst-chart.R
d1 <-
  d |>
  pivot_longer(Pol1:Harm3) |>
  group_by(name) |>
  # mutate(value_sc = (value) / (max(value)),
  #        value_sc = ifelse(grepl("Harm", name), 1 - value_sc, value_sc)) |>
  mutate(value_log = log10(value + min(value[value > 0])),
         value_sc  = (value_log - min(value_log)) / (max(value_log) - min(value_log)),
         value_sc  = ifelse(grepl("Harm", name), 1 - value_sc, value_sc)) |> 
  #--make biggest value 1
  mutate(value_sc = ifelse(value_sc > 1, 1, value_sc),
         value_sc = ifelse(is.infinite(value_sc), 1, value_sc)) |> 
  ungroup() |>
  #--change names
  mutate(name = case_when(
    name == "Pol1" ~ "HoneyBees",
    name == "Pol2" ~ "BumbleBees",
    name == "Pol3" ~ "HoverFlies",
    
    name == "PCont1" ~ "FarmlandBirds",
    name == "PCont2" ~ "Carabids",
    name == "PCont3" ~ "ParasitoidWasps",
    
    name == "Harm1" ~ "CropCoop",
    name == "Harm2" ~ "HarvestCoop",
    name == "Harm3" ~ "LongtermCoop",
    TRUE ~ name
  )) |> 
  mutate(name = factor(name, levels = name_levels)) |>
  left_join(species_lu, by = "Species") |>
  left_join(crop_lu, by = "Crop") |>
  left_join(loc_lu, by = "Location") |>
  mutate(
    Latin         = coalesce(Latin, Species),
    Crop_name     = coalesce(Crop_name, Crop),
    Location_name = coalesce(Location_name, Location),
    combo         = paste0(Crop_name, " (", Location_name, ")"),
    rid           = row_number()
  )

# d2 <- 
#   d1 |> 
#   group_by(Species) |> 
#   summarise(mean_value = mean(value_sc))
# 
# d2 |> 
#   arrange(-mean_value)

ALL_COMBO <- "__all__"
ALL_LABEL <- "Display all crop (locations)"

# Jitter is computed once so points don't move when the species changes
set.seed(42)
d1 <- d1 |>
  mutate(x_jit = as.numeric(name) + runif(n(), -0.3, 0.3),
         hover = paste0("Species: ", Species, " (", Latin, ")",
                        "<br>Crop: ", Crop_name,
                        "<br>Location: ", Location_name,
                        "<br>", name, " (scaled): ", round(value_sc, 3)))

species_choices <-
  d1 |>
  distinct(Species, Latin) |>
  arrange(Species) |>
  mutate(label = paste0(Species, " \u2013 ", Latin))

species_choices <- setNames(species_choices$Species, species_choices$label)

fmt_num <- function(x) {
  format(signif(x, 3), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------
ui <- page_navbar(
  title = "Weed Checks - What is my weed worth?",
  theme = bs_theme(version = 5),
  fillable = FALSE,
  
  nav_panel(
    "Species explorer",
    
    # Row 1: species selector (top left) + summary text
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Select a species and crop (location)"),
        selectInput(
          "species", label = NULL,
          choices = species_choices,
          selected = species_choices[1],
          width = "100%",
          selectize = TRUE
        ),
        selectInput(
          "combo", label = "Crop (location)",
          choices = setNames(ALL_COMBO, ALL_LABEL),
          selected = ALL_COMBO,
          width = "100%"
        ),
        class = "overflow-visible",
        style = "overflow: visible;"
      ),
      card(
        card_header("Summary"),
        uiOutput("summary_text")
      )
    ),
    
    # Row 2: fig1 (interactive) + fig2 (polar)
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("All species (selected species in red)"),
        plotlyOutput("fig1", height = "480px")
      ),
      card(
        card_header("Selected species"),
        plotOutput("fig2", height = "480px")
      )
    )
  )
)

# ------------------------------------------------------------------
# Server
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  species_data <- reactive({
    req(input$species)
    d1 |> filter(Species == input$species)
  })
  
  # Update the crop (location) choices whenever the species changes
  observeEvent(input$species, {
    combos <- sort(unique(species_data()$combo))
    updateSelectInput(session, "combo",
                      choices = c(setNames(ALL_COMBO, ALL_LABEL), setNames(combos, combos)),
                      selected = ALL_COMBO)
  })
  
  # TRUE when a specific crop (location) is chosen (and is valid for this species)
  combo_chosen <- reactive({
    !is.null(input$combo) &&
      input$combo != ALL_COMBO &&
      input$combo %in% species_data()$combo
  })
  
  # Data for the selected species (and crop (location), if one is chosen)
  sel_data <- reactive({
    sp <- species_data()
    if (combo_chosen()) filter(sp, combo == input$combo) else sp
  })
  
  # ---- Summary text -------------------------------------------------
  output$summary_text <- renderUI({
    sd <- sel_data()
    req(nrow(sd) > 0)
    
    latin <- sd$Latin[1]
    
    # crops and the locations that have data for each
    crop_loc <-
      sd |>
      distinct(Crop_name, Location_name) |>
      group_by(Crop_name) |>
      summarise(locs = paste(sort(Location_name), collapse = ", "), .groups = "drop") |>
      mutate(txt = paste0(Crop_name, " (", locs, ")"))
    
    n_obs <- sd |> distinct(Crop, Location) |> nrow()
    single <- combo_chosen()
    
    ranges <-
      sd |>
      group_by(name) |>
      summarise(min = min(value), max = max(value), .groups = "drop") |>
      mutate(txt = ifelse(min == max,
                          fmt_num(min),
                          paste0(fmt_num(min), " \u2013 ", fmt_num(max))))
    
    tagList(
      p(tags$strong(input$species), " (", tags$em(latin), ") has data for ",
        n_obs, " crop \u00d7 location combination", if (n_obs != 1) "s", ".",
        if (single) " Showing the selected crop (location) only."),
      p(tags$strong("Crops (locations): "), paste(crop_loc$txt, collapse = "; ")),
      p(tags$strong(if (single) "Value for each variable:"
                    else "Range of values for each variable:")),
      tags$ul(
        style = "columns: 3; margin-bottom: 0;",
        lapply(seq_len(nrow(ranges)), function(i) {
          tags$li(tags$strong(as.character(ranges$name[i])), ": ", ranges$txt[i])
        })
      )
    )
  })
  
  # ---- fig1: interactive jitter plot ----------------------------------
  output$fig1 <- renderPlotly({
    sd <- sel_data()
    others <- d1 |> filter(!(rid %in% sd$rid))
    
    p <-
      ggplot() +
      geom_point(data = others,
                 aes(x_jit, value_sc, text = hover),
                 color = "black", alpha = 0.45, size = 1.6) +
      geom_point(data = sd,
                 aes(x_jit, value_sc, text = hover),
                 color = "red", size = 3) +
      scale_x_continuous(breaks = seq_along(name_levels),
                         labels = name_levels) +
      labs(x = NULL, y = "Scaled value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") |>
      layout(hoverlabel = list(align = "left"))
  })
  
  # ---- fig2: polar plot for the selected species ----------------------
  output$fig2 <- renderPlot({
    sd <- sel_data()
    
    means <-
      sd |>
      group_by(name) |>
      summarise(value_sc_mean = mean(value_sc), .groups = "drop")
    
    ggplot() +
      geom_col(data = means,
               aes(name, value_sc_mean, fill = name),
               width = 1, color = "white") +
      geom_point(data = sd,
                 aes(name, value_sc, fill = name),
                 shape = 21, color = "black", size = 3) +
      scale_fill_manual(values = name_colors, drop = FALSE) +
      coord_polar() +
      labs(x = NULL, y = NULL, fill = NULL,
           title = paste0(input$species, " \u2013 ", sd$Latin[1],
                          if (combo_chosen()) paste0("\n", input$combo))) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "italic"))
  })
}

shinyApp(ui, server)
