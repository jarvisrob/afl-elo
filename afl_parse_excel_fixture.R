
ParseExcelFixture <- function(excel_file_path, ground_dictionary) {
  
  # Requires readxl package
  require(tidyverse)
  require(readxl)
  
  xls_raw <- data.frame(read_excel(excel_file_path))
  fixture <- xls_raw %>% filter(Home == "H") %>% separate(Round, c("RoundIgnore", "rnd"), sep = " ", convert = TRUE) 
  fixture$GroundStandard <- as.character(sapply(fixture$Ground, function(x, ground_dictionary) {gnd <- ground_dictionary[[x]]}, ground_dictionary))
  fixture <- fixture %>% dplyr::select(rnd, Team, Opponent, GroundStandard)
  names(fixture) <- c("rnd", "team.home", "team.away", "ground")
  
  fixture
}

ground_dictionary <- list("MCG" = "M.C.G.",
                          "Etihad Stadium" = "Docklands",
                          "Adelaide Oval" = "Adelaide Oval", 
                          "Cazaly's Stadium" = "Cazaly's Stadium",  # Note the special apostrophe on the index
                          "UNSW Canberra Oval" = "Manuka Oval",
                          "Perth Stadium" = "Perth Stadium",
                          "Gabba" = "Gabba",
                          "SCG" = "S.C.G.",
                          "Blundstone Arena" = "Bellerive Oval",
                          "GMHBA Stadium" = "Kardinia Park",
                          "Spotless Stadium" = "Sydney Showground",
                          "UTAS Stadium" = "York Park",
                          "Mars Stadium" = "Eureka Stadium",
                          "Jiangwan Stadium" = "Jiangwan Stadium",
                          "TIO Traeger Park" = "Traeger Park",
                          "Metricon Stadium" = "Carrara",
                          "TIO Stadium" = "Marrara Oval")



