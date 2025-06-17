library(readxl)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
data_edit_v2 <- read_excel("data/data_edit_v2.xlsx")

ff_metadata<-data_edit_v2 |> 
  select(Title, JournalSource,Year...5 ,Abstract, DOI, URL, NewLatitude, NewLongitude, RelevantSpecies)

ff_metadata <-ff_metadata |> 
  group_by(Title, RelevantSpecies) |>
  distinct() 

ff_metadata <- ff_metadata %>%
  mutate(
    lat = as.numeric(NewLatitude),
    lon = as.numeric(NewLongitude),
    StudyID = row_number()
  )

# Expand rows for each species
species_long <- ff_metadata %>%
  separate_rows(RelevantSpecies, sep = ",\\s*") %>%
  mutate(RelevantSpecies = str_trim(RelevantSpecies))

# Create a color palette per species
unique_species <- sort(unique(species_long$RelevantSpecies))
palette <- colorFactor("Set2", domain = unique_species)

# Create base leaflet map
m <- leaflet() %>%
  addTiles()

# Add species as separate layers
for (sp in unique_species) {
  species_data <- species_long %>% filter(RelevantSpecies == sp)
  
  m <- m %>%
    addCircleMarkers(
      data = species_data,
      lng = ~lon,
      lat = ~lat,
      color = ~palette(RelevantSpecies),
      radius = 6,
      fillOpacity = 0.7,
      popup=~paste0(
        "<strong>", Title, "</strong><br>",
        "Species: ", RelevantSpecies, "<br>",
        ifelse(!is.na(URL),
               paste0("<a href='", URL, "' target='_blank'>View paper</a>"),
               ifelse(!is.na(DOI),
                      paste0("<a href='https://doi.org/", DOI, "' target='_blank'>DOI link</a>"),
                      "No link available")
      )),
      group = sp
    )
}

# Add layer control
m <- m %>%
  addLayersControl(
    overlayGroups = unique_species,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addControl(
    html = "<div style='background:white; padding:5px; border-radius:5px;'>
              Click species to show/hide
            </div>",
    position = "topright"
  )

m
# Save the map
htmlwidgets::saveWidget(m, "index.html", selfcontained = TRUE)