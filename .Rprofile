source("renv/activate.R")

if (requireNamespace("conflicted", quietly = TRUE)) {
# Set conflict preferences
library(conflicted)
conflicts_prefer(dplyr::filter(), 
                 dplyr::select(),
                 readxl::read_xlsx(),
                 flextable::align(),
                 flextable::compose()
                 )
}
