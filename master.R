library(targets)
source("R_functions/figure_functions.R")

# Build plots
tar_make()

# Make figurindex.Rmd file
file.create(file = file.path("bookdown", "figurindex.Rmd"))

# Remove old web page
unlink(file.path("docs"), recursive = TRUE)

# Render book to get `reference-keys.txt`
bookdown::render_book(input = "bookdown", output_dir = "../docs")

# Generate figure index
gen_figindex(
    file.path("docs", "reference-keys.txt"), 
    file.path("bookdown", "figurindex.Rmd")
)

# Render book again... (to incorporate new figure index)
bookdown::render_book(input = "bookdown", output_dir = "../docs")

# Create .nojekyll file
file.create(file = file.path("docs", ".nojekyll"))

# Change cross-references from arabic to alpha
change_crossrefs(
    c(
        file.path("docs", "strukturell-integration.html"),
        file.path("docs", "social-integration.html"),
        file.path("docs", "kulturell-integration.html"),
        file.path("docs", "politisk-integration.html"),
        file.path("docs", "adaption-framtidstro-och-hälsa.html"),
        file.path("docs", "figurindex.html")
    )
)

# Add modified plugin-bookdown.js
file.copy(
    from = file.path("bookdown", "plugin-bookdown.js"),
    to = file.path("docs", "libs", "gitbook-2.6.7", "js", "plugin-bookdown.js"),
    overwrite = TRUE
)
