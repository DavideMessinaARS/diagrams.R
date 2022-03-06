# # Create vector of ids for the pages
# create_id_page <- function() {
#   pokemon <- rcorpora::corpora("games/pokemon")$pokemon %>%
#     dplyr::filter(generation_id == "1") %>%
#     dplyr::select(name) %>%
#     dplyr::mutate(name = tolower(name)) %>%
#     tibble::deframe()
#   moods <- tolower(rcorpora::corpora("humans/moods")$moods)
#   return(unique(ids::ids(200, moods, pokemon))[0:100])
# }
# 
# # Create vector of ids for the objects/cells
# create_id_cells <- function(n) {
#   pokemon <- rcorpora::corpora("games/pokemon")$pokemon %>%
#     dplyr::filter(generation_id == "2") %>%
#     dplyr::select(name) %>%
#     tibble::deframe()
#   moods <- tolower(rcorpora::corpora("humans/moods")$moods)
#   return(unique(ids::ids(n * 2, moods, pokemon))[0:n])
# }

create_unique_ids <- function(gen, existing) {
  
  ids_in_use <- if (missing(existing)) c() else existing
  
  pokemon <- rcorpora::corpora("games/pokemon")$pokemon %>%
    dplyr::filter(generation_id == gen) %>%
    dplyr::select(name) %>%
    dplyr::mutate(name = tolower(name)) %>%
    tibble::deframe()
  
  moods <- rcorpora::corpora("humans/moods")$moods
  
  function(n) {
    
    new_ids <- c()
    while (length(new_ids) < n) {
      n2_ids <- unique(ids::ids(n * 2, moods, pokemon))
      really_new_ids <- setdiff(n2_ids, ids_in_use)
      new_ids <- c(new_ids, really_new_ids[0:min(n, length(really_new_ids))])
    }
    
    ids_in_use <<- c(ids_in_use, new_ids)
    new_ids
  }
}
