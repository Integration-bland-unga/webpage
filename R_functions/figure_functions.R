##
# General functions

# Dictionary for common abbreviations
dictionary <- function(old){
    library(Dict)
    
    # Generate dictionary terms
    dict <- Dict$new(
                "SVF" = "Svenskfödda\nföräldrar",
                "Gen 1" = "Generation 1",
                "Gen 2" = "Generation 2",

                "14 (300 kr)" = "14 år\n(300 kr)",
                "15 (1000 kr)" = "15 år\n(1,000 kr)",
                "16 (1000 kr)" = "16 år\n(1,000 kr)",
                "19 (1000 kr)" = "19 år\n(1,000 kr)",
                "19 (15000 kr)" = "19 år\n(15,000 kr)",

                "0-20% inv" = "0-20%\ninv.",
                "21-49% inv" = "21-49%\ninv.",
                "50-100% inv" = "50-100%\ninv.",

                "  19 år" = "19 år",
                "F" = "Föräldrar",
                "B" = "Barn",

                "1" = "Inte alls stor",
                "2" = "Inte så stor",
                "3" = "Ganska stor",
                "4" = "Mycket stor",

                "swe_open" = "Svenskar bör vara öppna för\nkultur och traditioner som\ninvandrare kommer med",
                "swe_keep" = "Svenskar ska göra allt för\natt behålla sin kultur och\nsina traditioner",
                "imm_keep" = "Invandrare ska göra allt de\nkan för att behålla sin\nkultur och sina traditioner",
                "imm_adapt" = "Invandrare ska anpassa sig\ntill det svenska samhället",

                "Vecka" = "En/flera gånger\ni veckan",
                "Månad" = "En/flera gånger\ni månaden"
            )

    # If input is other than string, convert to string
    if (class(old) != "character"){
        old <- as.character(old)
    }

    # Create output list
    new <- unlist(lapply(old, function(x) dict[x, default = NA_character_]))

    # Replace NAs by original values
    dt <- as.data.table(list(new, old))
    setnames(dt, c("new", "old"))
    dt[is.na(new), new := old]

    return(dt[, new])

}

# Load data table
load_data_table <- function(f_path, names, dictionary_cols = NULL, 
                            factor_cols = NULL, remove_rows = NULL){
    dt <- fread(f_path)
    setnames(dt, new = names)
    
    # Change names using dictionary
    if (class(dictionary_cols) == "character"){
        dt[, (dictionary_cols) :=   lapply(.SD, function(x) dictionary(x)), 
                                    .SDcols = dictionary_cols]
    }

    if (class(factor_cols) == "character"){
        dt[, (factor_cols) :=   lapply(.SD, function(x) factor(x, levels = unique(x))), 
                                .SDcols = factor_cols]
    }

    # Remove rows // 
    # requires input of type list(list(col1, val1), list(col2, val2))
    if (class(remove_rows) == "list") {
        for (j in remove_rows) {
            dt <- dt[get(j[[1]][1]) != j[[2]][1]]
        }
    }

    return(dt)

}

# Reverse factor levels for giving factors in data.table
reverse_sort <- function (dt, factor) {
    library(forcats)
    dt[, (factor) := fct_rev(get(factor))]
    setorderv(dt, c(factor))
    return(dt)
}

# Rename factor levels in a data.table
set_level_names <- function(dt, factor, names){
    library(data.table)
    dt[, setattr(get(factor), "levels", names)]

    return(dt)

}

##
# Plotting functions

# Plot theme for dot plots
dot_plot_theme <- function(font = "Fengardo Neue") {
    library(ggplot2)
    library(ggthemes)

    return(theme_tufte() %+replace%
        theme(panel.border = element_rect(colour = "#3f6771", fill = NA, size = 0.2), 
            text = element_text(family = font, size = 16, color = "#151515"),
            panel.grid.major = element_line(color = alpha("#3f6771", 0.4),
                                             size = 0.1),
            legend.title = element_blank(),
            axis.title = element_blank(),
            axis.text = element_text(color = "#151515", size = 14),
            axis.ticks = element_line(color = "#3f6771"),
            plot.margin = margin(10, 10, 10, 0)
        ) 
    )

}


# Palette function
dot_palette <- function(n = 5, type = "discrete"){
    library(RColorBrewer)

    # Create divering color scale if type == diverging
    if (type == "diverging"){
        pal <- brewer.pal(n, "RdYlGn")

        # Replace weak contrast yellow
        pal <- replace(pal, pal ==  "#FFFFBF", "#e6e6aa")
        return(pal)
    }

    # Use Dark2 color scale if data is not divergin
    if (n <= 2){return(brewer.pal(3, "Dark2")[1:n])}
    if (n>2){return(brewer.pal(n, "Dark2"))}
}

# Main plotting function for dot_plots
dot_plot <- function(dt, x, y, color_var, breaks = waiver(), 
                    direction = "standing", y_limits = NULL, limits = NULL,
                    labels = scales::label_percent(accuracy = 1), 
                    color_values = dot_palette(length(unique(dt[,get(color_var)])))){
    library(ggplot2)
    library(forcats)
 
    # Reverse factor for plot layout for laying plots
    if (direction == "laying"){
        
        # Reverse factor for legend and axis order and set color limits
        if (x == color_var) {
            dt[, (x) := fct_rev(get(x))]
        } else if (x != color_var) {
            dt[, (color_var) := fct_rev(get(color_var))]
        }
        # Set color limits
        if (class(limits) == "NULL") {
            limits <- unique(dt[,get(color_var)])
        }
    }

    # Initial plot
    p <- ggplot(dt, aes(x = get(x), y = get(y))) +
            geom_point(color = "black", size = 2.8, pch = 21, fill = alpha("black", 0.0)) +  
            geom_point(aes(color = get(color_var)), size = 2.6, alpha = 0.7) +
            dot_plot_theme() +
            scale_color_manual(values = color_values,
                                limits = limits) + 
            scale_y_continuous(labels = labels, breaks = breaks, limits = y_limits)

    if (direction == "laying"){
        p <- p + theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = alpha("#3f6771", 0.6), linetype = "dashed", size = 0.1)
        )
    } else {
        p <- p + theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(colour = alpha("#3f6771", 0.6), linetype = "dashed", size = 0.1)
        )
    }

    return(p)

}

line_plot <- function(dt, x, y, group_var = NULL){
    library(ggplot2)

    # Initial plot
    p <- ggplot(dt, aes(x = get(x), y = get(y), group = get(group_var), color = get(group_var))) + 
            geom_line() + 
            dot_plot_theme() +
            scale_color_manual(values=dot_palette(length(unique(dt[,get(group_var)]))))
    
    return(p)
}

##
# Bookdown functions

# This function find reference to figures and changes the format from 1.1 to 1a,
# 1.2 to 1b et.c
change_crossrefs <- function (file_paths) {

    for (path in file_paths) {

        tx <- readLines(path)

        # Replace figure cross-references for figurindex
        if (grepl("figurindex", path)) {
            for (i in 30:1) {
                tx <-   gsub(
                            paste0("(<p>.+\">[1-9])(\\.", i, ")"),
                            paste0("\\1", letters[i]),
                            tx
                        )
            }
        # Replace figure cross-references for all other pages 
        } else {
            for (i in 30:1) {
                tx <-   gsub(
                            paste0("(Figur\\s.+\">[1-9])", "(\\.",i, ")(<\\/a>)"),
                            paste0("\\1", letters[i], "\\3"),
                            tx
                        )
            }
        }

        writeLines(tx, con = path)
    }
    return(file_paths)
}

# Generates a list of figures that are included in a .Rmd file.
# Run on one chapter to format the list with Rmarkdown 
gen_figlist <- function (fig_list, first_fig, last_fig, file_path, outfile) {
    library(stringr)
    library(readr)

    # For each figure in figlist
    for (fig in fig_list[first_fig:last_fig]) {
        # Extract fig.cap
        fig_cap <- str_match(
                        read_file(file_path),
                        paste0("(r\\s", substring(fig, 5), ".+fig.cap.+\")(.+)(\"\\})")
                    )[[3]]
        # Create text entry
        cat(
            paste0("\\@ref(",fig, ") - ", fig_cap, "\n"),
            file = outfile,
            sep = "\n",
            append = TRUE
        )
    }

    return(outfile)

}

# Generates the entire figureindex.Rmd. 
# It creates headings and fetches formated figure lists using `gen_figlist()`
gen_figindex <- function (reference_keys, outfile) {

    fig_list <- scan(reference_keys, character())
    
    # Create empty file
    cat("", file = outfile)

    # Page title
    cat("# Figurindex {-}\n", file = outfile, sep = "\n", append = TRUE)
    
    # Strukturell integration
    cat("### Strukturell integration {-}\n", file = outfile, sep = "\n", append = TRUE)
    
    # Write list of figures
    gen_figlist(fig_list, 1, 17, 
                file_path = file.path("bookdown", "01-strukturell_integration.Rmd"), 
                outfile = outfile)

    # Social integration
    cat("### Social integration {-}\n", file = outfile, sep = "\n", append = TRUE)

    # Write list of figures
    gen_figlist(fig_list, 18, 35, 
                file_path = file.path("bookdown", "02-social_integration.Rmd"), 
                outfile = outfile)

    # Kulturell integration
    cat("### Kulturell integration {-}\n", file = outfile, sep = "\n", append = TRUE)
    # Write list of figues
    gen_figlist(fig_list, 36, 59, 
                file_path = file.path("bookdown", "03-kulturell_integration.Rmd"), 
                outfile = outfile)

    # Politisk integration
    cat("### Politisk integration {-}\n", file = outfile, sep = "\n", append = TRUE)
    # Write list of figures
    gen_figlist(fig_list, 60, 62, 
                file_path = file.path("bookdown", "04-politisk_integration.Rmd"), 
                outfile = outfile)


    # Adaption
    cat("### Adaption, framtidstro och hälsa{-}\n", file = outfile, sep = "\n", append = TRUE)
    # Write list of figures 
    gen_figlist(fig_list, 63, 72, 
                file_path = file.path("bookdown", "05-framtidstro.Rmd"), 
                outfile = outfile)

    return(outfile)
}