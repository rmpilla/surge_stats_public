
sort_bib <- function(refs_file){

  # read .bib file and sort alphabetically
df <- bib2df(refs_file) %>% # read file
  arrange(BIBTEXKEY) # sort

# write sorted file
df2bib(df %>% arrange(BIBTEXKEY), file = refs_file)

}