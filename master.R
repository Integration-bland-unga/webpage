library(targets)

# Build plots
tar_make()

# Render book
bookdown::render_book(input = "bookdown", output_dir = "../docs")



# Change cross-references
change_crossrefs(
    c(
        file.path("docs", "strukturell-integration.html"),
        file.path("docs", "social-integration.html"),
        file.path("docs", "kulturell-integration.html"),
        file.path("docs", "politisk-integration.html"),
        file.path("docs", "adaption-framtidstro-och-hälsa.html")
    )
)