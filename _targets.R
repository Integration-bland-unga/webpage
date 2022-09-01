library(targets)
source("R_functions/figure_functions.R")

tar_option_set(
    packages = c("data.table", "fst", "forcats")
)


list(
    ###
    # Structural integration

    # Cash margin
    tar_target(
        dt_f1d1,
        load_data_table(
            file.path("data", "f4_2_kontant.csv"),
            names = c("age", "background", "proportion", "lb", "ub"),
            dictionary_cols = c("background", "age"),
            factor_cols = c("background", "age")
        ),
        format = "fst_dt"
    ),
    tar_target(p_f1d1, bar_plot(dt_f1d1, "background", "proportion",
        color_var = "background",
        breaks = seq(0, 1, 0.2),
        y_limits = c(0,1),
        direction = "laying"
    ) +
        theme(
            strip.text.y.left = element_text(angle = 0),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        ) +
        coord_flip() + facet_grid(rows = vars(age), switch = "both")),

    # Do stuff with friend
    tar_target(dt_f1d2, load_data_table(file.path("data", "f4_3_vanner.csv"),
        names = c("background", "age", "response", "proportion", "lb", "ub"),
        dictionary_cols = c("background"),
        factor_cols = c("background", "response")
    ),
    format = "fst_dt"
    ),
    tar_target(p_f1d2, bar_plot(dt_f1d2, "age", "proportion",
        color_var = "response",
        breaks = seq(0, 1, 0.1),
        color_values = dot_palette(length(unique(dt_f1d2[, response])), type = "diverging")
    ) +
        facet_grid(cols = vars(background))),

    ###
    # Social integration

    # Family type by origin
    tar_target(dt_famtyp_orig, 
        load_data_table(file.path("data", "f5_1_famtyp_orig.csv"),
        names = c("origin", "family_type", "proportion", "lb", "ub"),
        factor_cols = c("origin", "family_type")
        ),
        format = "fst_dt"
    ),
    tar_target(p_famtyp_orig, 
        bar_plot(
            dt_famtyp_orig, 
            "origin", "proportion", 
            color_var = "family_type"
        ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7))
    ),

    # Joint physical custody
    tar_target(dt_vaxbo, 
        load_data_table(file.path("data", "f5_2_vaxbo_gen_orig_kon.csv"),
        names = c("grouping", "background", "proportion", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("grouping", "background")
    ),
        format = "fst_dt"
    ),
    tar_target(p_vaxbo, bar_plot(dt_vaxbo, "background", "proportion",
        color_var = "background",
        direction = "laying", color_values = c(dot_palette(3), dot_palette(6))
    ) +
        theme(strip.text.y = element_blank(), legend.position = "none") +
        facet_grid(
            rows = vars(grouping),
            scales = "free",
            space = "free"
        ) + 
        coord_flip()
    ),

    # Family indicator indices
    tar_target(dt_famindex, 
        load_data_table(file.path("data", "f5_3_famindex_gen.csv"),
        names = c("measure", "background", "value", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("measure", "background")
    ), format = "fst_dt"),
    tar_target(p_famindex, bar_plot(
        dt_famindex[, ":="(
            value = value * 100,
            lb = lb * 100,
            ub = ub * 100
        )],
        "background", "value",
        color_var = "background",
        direction = "laying",
        labels = waiver(),
        breaks = seq(0, 100, 10),
        y_limits = c(0, 100)
    ) +
        theme(
            strip.text.y.left = element_text(angle = 0, hjust = 1),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom"
        ) +
        facet_grid(rows = vars(measure), switch = "both") + coord_flip()),

    # Foreign background friends
    tar_target(dt_avg_foregin_friends,
        load_data_table(file.path("data", "f5_4_fimm_gen_kon_share.csv"),
        names = c("gender", "background", "proportion", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("gender", "background")
        ), 
        format = "fst_dt"
    ),
    tar_target(p_avg_foreign_friends, 
        bar_plot(
            dt_avg_foregin_friends,
            "background", "proportion",
            color_var = "background",
            direction = "laying"
        ) +
        facet_grid(rows = vars(gender)) +
        theme(
            legend.position = "bottom",
            strip.text.y = element_text(angle = 0)
        ) +
        coord_flip()),

    # Foreign background friends by percentage Gen 1
    tar_target(dt_avg_foreign_friends_by_percentage_gen1,
        load_data_table(
            file.path("data", "f5_5_fimm_gen_kon_UBak8_sha.csv"),
            names = c(
                "gender",
                "composition",
                "background",
                "proportion",
                "lb",
                "ub"
            ),
            dictionary_cols = c("background", "composition"),
            factor_cols = c("gender", "composition", "background")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_avg_foreign_friends_by_percentage_gen1,
        bar_plot(dt_avg_foreign_friends_by_percentage_gen1, 
            "composition", "proportion",
            color_var = "background",
            breaks = seq(0, 1, 0.1)
        ) +
            facet_grid(cols = vars(gender)) +
            theme(
                axis.text.x = element_text(size = 12),
                legend.position = "bottom", 
                plot.margin = margin(10, 10, 10, 10)
            )
    ),

    # Foreign friends by age
    tar_target(dt_foreign_friends_age, 
        load_data_table(file.path("data", "f5_6_futgr_gen_age.csv"),
            names = c("age", "background", "proportion", "lb", "ub"),
            dictionary_cols = "background",
            factor_cols = "background"
        ),
        format = "fst_dt"
    ),
    tar_target(p_foreign_friends_age, 
        line_plot(dt_foreign_friends_age[, age := as.integer(substr(age, 1, 2))], 
            "age", "proportion", 
            group_var = "background"
        ) +
        scale_y_continuous(
            labels = scales::label_percent(accuracy = 1),
            limits = c(0, 0.4)
        ) +
        scale_x_continuous(
            breaks = c(14, 16, 19),
            labels = c("14 år", "16 år", "19 år")
        ) +
        theme(legend.position = "bottom")
    ),

    # Proportion with boy-/girlfriend in in/out group
    tar_target(dt_ak9_partner,
        load_data_table(file.path("data", "f5_9_date_gen_kon.csv"),
            names = c("gender", "background", "partner", "proportion", "lb", "ub"),
            dictionary_cols = c("background"),
            factor_cols = c("gender", "partner", "background")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_ak9_partner,
        bar_plot(dt_ak9_partner, "background", "proportion",
            color_var = "partner",
            breaks = seq(0, 1, 0.1)
        ) +
            facet_grid(cols = vars(gender)) +
            theme(
                axis.text.x = element_text(size = 13),
                legend.position = "bottom"
            )
    ),

    # Social activities per year
    tar_target(dt_social_gender_background, load_data_table(file.path("data", "f5_14_lta_gen_kon.csv"),
        names = c("gender", "background", "index", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("gender", "background")
    ), format = "fst_dt"),
    tar_target(p_social_gender_background, bar_plot(dt_social_gender_background, "background", "index",
        color_var = "background", direction = "laying", labels = waiver()
    ) +
        facet_grid(rows = vars(gender)) +
        theme(legend.position = "bottom", strip.text.y = element_text(angle = 0)) +
        coord_flip()),

    # Sports/music/drama club
    tar_target(dt_sports, load_data_table(file.path("data", "f5_15_lta_gen_kon_age.csv"),
        names = c("age", "background", "mean", "lb", "ub"),
        dictionary_cols = c("background", "age"),
        factor_cols = "background"
    ), format = "fst_dt"),
    tar_target(p_sports, line_plot(dt_sports[, age := as.integer(substr(age, 1, 2))], "age", "mean", group_var = "background") +
        scale_y_continuous(
            labels = c("Aldrig", "Mer sällan", "Varje månad", "Varje vecka"),
            breaks = c(0, 1, 2, 3), limits = c(0, 3)
        ) +
        scale_x_continuous(breaks = c(14, 16, 19), labels = c("14 år", "16 år", "19 år")) +
        theme(legend.position = "bottom")),

    # In club with foreign background members
    tar_target(dt_club_foreign, load_data_table(file.path("data", "f5_16_gen_kon_cond.csv"),
        names = c("gender", "background", "mean", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("gender", "background")
    ), format = "fst_dt"),
    tar_target(p_club_foreign, bar_plot(dt_club_foreign, "background", "mean",
        color_var = "background", direction = "laying",
        labels = c("Aldrig", "Mer sällan", "Varje månad", "Varje vecka", "Varje dag"),
        breaks = c(0, 1, 2, 3, 4), y_limits = c(0, 4)
    ) +
        facet_grid(rows = vars(gender)) +
        theme(legend.position = "bottom", strip.text.y = element_text(angle = 0)) +
        coord_flip()),

    # Victim of tesing, bullying and being scared
    tar_target(dt_victim, load_data_table(file.path("data", "f5_17_vict_ubak8_kon_gen.csv"),
        names = c("background", "gender", "composition", "mean", "lb", "ub"),
        dictionary_cols = c("background", "composition"),
        factor_cols = c("gender", "background", "composition")
    ), format = "fst_dt"),
    tar_target(p_victim, bar_plot(dt_victim, "composition", "mean",
        color_var = "composition",
        labels = c("Aldrig", "En ibland (11)", "Två ibland (22)"),
        breaks = c(0, 0.11, 0.22), y_limits = c(0, 0.25),
        color_values = dot_palette(length(unique(dt_victim[, composition])), type = "diverging")
    ) +
        theme(
            plot.title = element_blank(),
            legend.position = "bottom",
            axis.text.x = element_blank(),
            plot.margin = margin(10, 5, 30, 0)
        ) +
        facet_grid(gender ~ background)),
    # Discrimination by place
    tar_target(dt_f5_18, rbind(
        load_data_table(file.path("data", "skolo.csv"),
            names = c("origin", "category", "mean", "lb", "ub"),
            factor_cols = c("origin", "category")
        )[, place := "I skolan"],
        load_data_table(file.path("data", "trafiko.csv"),
            names = c("origin", "category", "mean", "lb", "ub"),
            factor_cols = c("origin", "category")
        )[, place := "I lokaltrafiken"],
        load_data_table(file.path("data", "butiko.csv"),
            names = c("origin", "category", "mean", "lb", "ub"),
            factor_cols = c("origin", "category")
        )[, place := "I butiker, caféer osv."],
        load_data_table(file.path("data", "skolo.csv"),
            names = c("origin", "category", "mean", "lb", "ub"),
            factor_cols = c("origin", "category")
        )[, place := "Av polis och väktare"]
    ), format = "fst_dt"),
    tar_target(p_f5_18, bar_plot(set_level_names(
        reverse_sort(dt_f5_18, "place"), "category", c("Ofta/Alltid", "Ibland", "Aldrig")
    ),
    "origin", "mean",
    color_var = "category",
    y_limits = c(0, 1),
    color_values = dot_palette(length(unique(dt_f5_18[, category])), type = "diverging")
    ) +
        facet_wrap(vars(place), ncol = 2) +
        theme(plot.margin = margin(10, 10, 10, 10))),

    # Discrimination by origin
    tar_target(dt_discrimination_orig, load_data_table(file.path("data", "f5_19_pdisc4.csv"),
        names = c("origin", "frequency", "proportion", "lb", "ub"),
        factor_cols = c("origin", "frequency"),
        dictionary_cols = "frequency"
    ), format = "fst_dt"),
    tar_target(p_discrimination_orig, bar_plot(dt_discrimination_orig, "origin", "proportion",
        color_var = "frequency", y_limits = c(0, 0.6),
        color_values = dot_palette(length(unique(dt_discrimination_orig[, frequency])), type = "diverging")
    ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7))),

    # Ethnic discrimination by origin
    tar_target(dt_discrimination_ethn, load_data_table(file.path("data", "f5_20_disc4etn.csv"),
        names = c("origin", "frequency", "proportion", "lb", "ub"),
        factor_cols = c("origin", "frequency")
    )[frequency == 1, frequency := "En/flera gånger\ni veckan"][
        frequency == 2, frequency := "En/flera gånger\ni månaden"
    ][
        frequency == 3, frequency := "Mer sällan"
    ][
        frequency == 4, frequency := "Aldrig"
    ], format = "fst_dt"),
    tar_target(p_discrimination_ethn, bar_plot(dt_discrimination_ethn, "origin", "proportion",
        color_var = "frequency", y_limits = c(0, 1),
        color_values = dot_palette(length(unique(dt_discrimination_ethn[, frequency])), type = "diverging")
    ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7))),

    # Attitudes to other groups
    tar_target(dt_f5_21, rbind(
        load_data_table(file.path("data", "attog_swex.csv"),
            names = c("ethnicity", "attitude", "proportion", "lb", "ub"),
            factor_cols = c("ethnicity", "attitude")
        )[, background := "Svensk bakgrund"],
        load_data_table(file.path("data", "attog_imex.csv"),
            names = c("ethnicity", "attitude", "proportion", "lb", "ub"),
            factor_cols = c("ethnicity", "attitude")
        )[, background := "Utländsk bakgrund"]
    ),
    format = "fst_dt"
    ),
    tar_target(p_f5_21, bar_plot(dt_f5_21, "ethnicity", "proportion",
        color_var = "attitude",
        y_limits = c(0, 1),
        direction = "laying",
        color_values = dot_palette(length(unique(dt_f5_21[, attitude])), type = "diverging")
    ) +
        facet_grid(rows = vars(background), scales = "free") +
        coord_flip() +
        theme(
            legend.position = "right",
            plot.margin = margin(10, 10, 10, 10),
            panel.spacing = unit(1.5, "lines"),
            axis.text.y = element_text(size = 12)
        )
    ),

    ###
    # Kulturell integration

    # Identification with Sweden by generation
    tar_target(dt_identification_gen,
        load_data_table(file.path("data", "f6_1_id_fig_y_gen.csv"),
            names = c("background", "category", "proportion", "lb", "ub"),
            dictionary_cols = c("background", "category"),
            factor_cols = c("background", "category")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_identification_gen,
        bar_plot(dt_identification_gen[, category := forcats::fct_rev(category)],
            "background", "proportion",
            color_var = "category",
            breaks = seq(0, 1, 0.1),
            color_values = rev(dot_palette(length(unique(dt_identification_gen[, category])), type = "diverging"))
        )
    ),

    # Identification by origin
    tar_target(dt_identification_orig,
        load_data_table(file.path("data", "f6_2_id_gen_origmax.csv"),
            names = c("background", "generation", "category", "proportion", "lb", "ub"),
            dictionary_cols = c("background", "category", "generation"),
            factor_cols = c("background", "category", "generation")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_identification_orig,
        bar_plot(reverse_sort(dt_identification_orig, "generation"),
            "generation", "proportion",
            color_var = "category",
            direction = "laying", breaks = seq(0, 1, 0.1),
            color_values = dot_palette(length(unique(dt_identification_orig[, category])), type = "diverging")
        ) +
            coord_flip() + facet_grid(rows = vars(background)) +
            theme(
                strip.text.y.right = element_text(angle = 0),
                legend.position = "bottom"
            )
    ),

    # Identification by age
    tar_target(dt_identification_age, load_data_table(file.path("data", "f6_3_id_bstack.csv"),
        names = c("background", "age", "category", "proportion", "lb", "ub"),
        dictionary_cols = c("background", "age", "category"),
        factor_cols = c("background", "category", "age")
    ), format = "fst_dt"),
    tar_target(
        p_identification_age,
        bar_plot(reverse_sort(dt_identification_age, "age"),
            "age", "proportion",
            color_var = "category",
            direction = "laying", breaks = seq(0, 1, 0.2),
            y_limits = c(0, 1),
            color_values = dot_palette(length(unique(dt_identification_age[, category])), type = "diverging")
        ) +
            coord_flip() + facet_grid(rows = vars(background)) +
            theme(
                strip.text.y.right = element_text(angle = 0),
                legend.position = "bottom"
            )
    ),

    # Identification with other group over age by background
    tar_target(dt_identification_other_age, load_data_table(file.path("data", "f6_4_idoc2_age_gen_no4.csv"),
        names = c("background", "age", "category", "proportion", "lb", "ub"),
        dictionary_cols = c("background", "age"),
        factor_cols = c("background", "category", "age")
    ), format = "fst_dt"),
    tar_target(
        p_identification_other_age,
        bar_plot(reverse_sort(dt_identification_other_age, "age"),
            "age", "proportion",
            color_var = "category",
            direction = "laying", breaks = seq(0, 1, 0.2),
            y_limits = c(0, 1),
            color_values = dot_palette(length(unique(dt_identification_other_age[, category])), type = "diverging")
        ) +
            coord_flip() + facet_grid(rows = vars(background)) +
            theme(
                strip.text.y.right = element_text(angle = 0),
                legend.position = "bottom"
            )
    ),

    # Identification with other group over age by origin
    tar_target(dt_identification_other_age_origin, load_data_table(file.path("data", "f6_5_idoc2_age_originmax_no4.csv"),
        names = c("origin", "age", "category", "proportion", "lb", "ub"),
        dictionary_cols = c("age"),
        factor_cols = c("origin", "category", "age")
    ),
    format = "fst_dt"
    ),
    tar_target(
        p_identification_other_age_origin,
        bar_plot(reverse_sort(dt_identification_other_age_origin, "age"),
            "age", "proportion",
            color_var = "category",
            direction = "laying", breaks = seq(0, 0.8, 0.2),
            y_limits = c(0, 0.8),
            color_values = dot_palette(length(unique(dt_identification_other_age_origin[, category])), type = "diverging")
        ) +
            coord_flip() + facet_grid(rows = vars(origin)) +
            theme(
                strip.text.y.right = element_text(angle = 0),
                legend.position = "bottom"
            )
    ),

    # Ethnic identification over age by background
    tar_target(dt_f6_6, set_level_names(
        load_data_table(file.path("data", "f6_6_berry123.csv"),
            names = c("background", "age", "category", "proportion", "lb", "ub"),
            dictionary_cols = c("age", "background"),
            factor_cols = c("background", "category", "age")
        ),
        "category",
        c("Ingen", "Annan", "Svensk", "Svensk och annan")
    ),
    format = "fst_dt"
    ),
    tar_target(
        p_f6_6,
        bar_plot(reverse_sort(dt_f6_6, "age"),
            "age", "proportion",
            color_var = "category",
            direction = "laying", breaks = seq(0, 1, 0.2),
            y_limits = c(0, 1),
            color_values = dot_palette(length(unique(dt_f6_6[, category])), type = "diverging")
        ) +
            coord_flip() + facet_grid(rows = vars(background)) +
            theme(
                strip.text.y.right = element_text(angle = 0),
                legend.position = "bottom"
            )
    ),


    # Ethnic identification over age by origin
    tar_target(dt_f6_7, set_level_names(
        load_data_table(file.path("data", "f6_7_berry123originmax.csv"),
            names = c("origin", "age", "category", "proportion", "lb", "ub"),
            dictionary_cols = c("age"),
            factor_cols = c("origin", "category")
        ),
        "category",
        c("Ingen", "Annan", "Svensk", "Svensk och annan")
    ),
    format = "fst_dt"
    ),
    tar_target(
        p_f6_7,
        bar_plot(reverse_sort(dt_f6_7, "age"),
            "age", "proportion",
            color_var = "category",
            direction = "laying", breaks = seq(0, 1, 0.2),
            y_limits = c(0, 0.6),
            color_values = dot_palette(length(unique(dt_f6_7[, category])), type = "diverging")
        ) +
            coord_flip() + facet_grid(rows = vars(origin)) +
            theme(
                strip.text.y.right = element_text(angle = 0),
                legend.position = "bottom"
            )
    ),

    # Swedish language skills
    tar_target(dt_swedish_skills, load_data_table(file.path("data", "f6_8_spr_gen.csv"),
        names = c("category", "background", "score", "lb", "ub"),
        dictionary_cols = c("background"),
        factor_cols = c("background")
    ),
    format = "fst_dt"
    ),
    tar_target(p_swedish_skills, bar_plot(dt_swedish_skills, "background", "score",
        color_var = "category",
        labels = waiver(), y_limits = c(0, 100)
    ) +
        theme(
            legend.position = "bottom",
            plot.margin = margin(10, 10, 10, 10)
        )),
    # Religion by background
    tar_target(dt_religion_background,
        load_data_table(file.path("data", "f6_9_rel1_migcat3.csv"),
            names = c("background", "religion", "proportion", "lb", "ub"),
            factor_cols = c("background", "religion")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_religion_background,
        bar_plot(dt_religion_background,
            "background", "proportion",
            color_var = "religion",
            breaks = seq(0, 1, 0.1),
            y_limits = c(0, 0.6)
        )
    ),

    # Religion by origin
    tar_target(dt_religion_origin,
        load_data_table(file.path("data", "f6_10_rel1_origmax_svf.csv"),
            names = c("origin", "religion", "proportion", "lb", "ub"),
            factor_cols = c("origin", "religion")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_religion_origin,
        bar_plot(dt_religion_origin,
            "origin", "proportion",
            color_var = "religion", breaks = seq(0, 1, 0.1)
        ) +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.7))
    ),

    # Importance of religion by background
    tar_target(dt_religion_importance_background,
        load_data_table(file.path("data", "f6_11_rel2_migcat3_EJ_INGEN.csv"),
            names = c("background", "importance", "proportion", "lb", "ub"),
            factor_cols = c("background", "importance")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_religion_importance_background,
        bar_plot(dt_religion_importance_background,
            "background", "proportion",
            color_var = "importance",
            color_values = dot_palette(length(unique(dt_religion_importance_background[, importance])), type = "diverging"),
            breaks = seq(0, 1, 0.1)
        )
    ),

    # Importance of religion by origin
    tar_target(dt_religion_importance_origin,
        load_data_table(file.path("data", "f6_12_rel2_origmax_EJ_INGEN.csv"),
            names = c("origin", "importance", "proportion", "lb", "ub"),
            factor_cols = c("origin", "importance")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_religion_importance_origin,
        bar_plot(dt_religion_importance_origin,
            "origin", "proportion",
            color_var = "importance",
            color_values = dot_palette(length(unique(dt_religion_importance_origin[, importance])), type = "diverging"),
            breaks = seq(0, 1, 0.1)
        ) +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.7))
    ),

    # Importance of religion by background and religion
    tar_target(dt_religion_importance_bgrel,
        load_data_table(file.path("data", "f6_13_e_rel.csv"),
            names = c("background", "religion", "importance", "proportion", "lb", "ub"),
            factor_cols = c("background", "religion", "importance"),
            dictionary_cols = c("background")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_religion_importance_bgrel,
        bar_plot(dt_religion_importance_bgrel[!(background == "Svenskfödda" & religion == "Muslim")],
            "religion", "proportion",
            color_var = "importance",
            color_values = dot_palette(length(unique(dt_religion_importance_bgrel[, importance])), type = "diverging"),
            breaks = seq(0, 1, 0.1)
        ) +
            facet_grid(cols = vars(background))
    ),

    # Importance of religion by background and religion, parents and children
    tar_target(dt_f6_14,
        load_data_table(file.path("data", "f6_14_intergen_rel.csv"),
            names = c("background", "generation", "importance", "proportion", "lb", "ub"),
            factor_cols = c("background", "generation", "importance"),
            dictionary_cols = c("generation")
        ),
        format = "fst_dt"
    ),
    tar_target(
        p_f6_14,
        bar_plot(dt_f6_14,
            "generation", "proportion",
            color_var = "importance",
            color_values = dot_palette(length(unique(dt_f6_14[, importance])), type = "diverging"),
            breaks = seq(0, 1, 0.1)
        ) +
            facet_grid(cols = vars(background)) +
            theme(
                legend.position = "bottom",
                plot.margin = margin(10, 30, 10, 10),
                axis.text.x = element_text(angle = 45, vjust = 0.7)
            )
    ),

    # Importance of religion by age
    tar_target(dt_religiosity_age, load_data_table(file.path("data", "f6_15_rel_age.csv"),
        names = c("age", "background", "mean", "lb", "ub"),
        dictionary_cols = c("age", "background"),
        factor_cols = "background"
    ), format = "fst_dt"),
    tar_target(p_religiosity_age, line_plot(dt_religiosity_age[, age := as.integer(substr(age, 1, 2))], "age", "mean", group_var = "background") +
        scale_y_continuous(
            labels = c("Inte alls viktig", "Inte så viktig", "Ganska viktigt", "Mycket viktigt"),
            breaks = c(0, 1, 2, 3), limits = c(0, 3)
        ) +
        scale_x_continuous(breaks = c(14, 15, 16, 19), labels = c("14 år", "15 år", "16 år", "19 år")) +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 2, byrow = TRUE))),

    # Family attidtudes by gender and background
    tar_target(dt_family_attitudes_genderbg, load_data_table(file.path("data", "f6_16_tol_migcat3_kon.csv"),
        names = c("issue", "gender", "background", "proportion", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("issue", "gender", "background")
    ), format = "fst_dt"),
    tar_target(
        p_family_attitudes_genderbg,
        bar_plot(dt_family_attitudes_genderbg, "gender", "proportion",
            color_var = "background",
            breaks = seq(0, 1, 0.1)
        ) +
            facet_grid(cols = vars(issue)) +
            theme(
                axis.text.x = element_text(size = 13),
                legend.position = "bottom",
                plot.margin = margin(10, 10, 10, 10)
            )
    ),

    # Family attidtudes by origin
    tar_target(dt_family_attitudes_origin, load_data_table(file.path("data", "f6_17_tol_origmax.csv"),
        names = c("issue", "origin", "proportion", "lb", "ub"),
        dictionary_cols = "origin",
        factor_cols = c("issue", "origin")
    ), format = "fst_dt"),
    tar_target(
        p_family_attitudes_origin,
        bar_plot(dt_family_attitudes_origin, "issue", "proportion",
            color_var = "origin",
            breaks = seq(0, 1, 0.1)
        )
    ),

    # Family attitudes over age
    tar_target(dt_family_attitudes_age, load_data_table(file.path("data", "f6_18_tolindex_mig3_age.csv"),
        names = c("age", "background", "mean", "lb", "ub"),
        dictionary_cols = c("age", "background"),
        factor_cols = "background"
    ), format = "fst_dt"),
    tar_target(p_family_attitudes_age, line_plot(dt_family_attitudes_age[, age := as.integer(substr(age, 1, 2))], "age", "mean", group_var = "background") +
        scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(0, 0.4, 0.05)) +
        scale_x_continuous(breaks = c(14, 16), labels = c("14 år", "16 år")) +
        theme(legend.position = "bottom")),

    # Gender attitudes by gender and background
    tar_target(dt_gender_attitudes_genderbg, load_data_table(file.path("data", "f6_19_grol_migcat3_kon.csv"),
        names = c("issue", "gender", "background", "proportion", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("issue", "gender", "background")
    ), format = "fst_dt"),
    tar_target(
        p_gender_attitudes_genderbg,
        bar_plot(dt_gender_attitudes_genderbg, "gender", "proportion",
            color_var = "background",
            breaks = seq(0, 1, 0.1)
        ) +
            facet_grid(cols = vars(issue)) +
            theme(
                axis.text.x = element_text(size = 13),
                legend.position = "bottom",
                plot.margin = margin(10, 10, 10, 10)
            )
    ),

    # Gender attidtudes by origin
    tar_target(dt_gender_attitudes_origin, load_data_table(file.path("data", "f6_20_grol_origmax.csv"),
        names = c("issue", "origin", "proportion", "lb", "ub"),
        dictionary_cols = "origin",
        factor_cols = c("issue", "origin")
    ), format = "fst_dt"),
    tar_target(
        p_gender_attitudes_origin,
        bar_plot(dt_gender_attitudes_origin, "issue", "proportion",
            color_var = "origin",
            breaks = seq(0, 1, 0.1), y_limits = c(0, 0.7)
        ) +
            theme(axis.text.x = element_text(size = 13), legend.position = "bottom") +
            guides(color = guide_legend(nrow = 2, byrow = TRUE))
    ),

    # Gender attitudes over age
    tar_target(dt_gender_attitudes_age, load_data_table(file.path("data", "f6_21_grolindex_mig3_age.csv"),
        names = c("age", "background", "mean", "lb", "ub"),
        dictionary_cols = c("age", "background"),
        factor_cols = "background"
    ), format = "fst_dt"),
    tar_target(p_gender_attitudes_age, line_plot(dt_gender_attitudes_age[, age := as.integer(substr(age, 1, 2))], "age", "mean", group_var = "background") +
        scale_y_continuous(
            labels = scales::label_percent(accuracy = 1),
            limits = c(0, 0.45),
            breaks = seq(0, 0.6, 0.05)
        ) +
        scale_x_continuous(
            breaks = c(14, 15, 19),
            labels = c("14 år", "15 år", "19 år")
        ) +
        theme(legend.position = "bottom")),

    # Attitudes concerning violence
    tar_target(dt_violence_attitudes_genderbg, load_data_table(file.path("data", "f6_22_masc_migcat3_kon.csv"),
        names = c("issue", "gender", "background", "proportion", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("issue", "gender", "background")
    ), format = "fst_dt"),
    tar_target(
        p_violence_attitudes_genderbg,
        bar_plot(dt_violence_attitudes_genderbg, "gender", "proportion",
            color_var = "background",
            breaks = seq(0, 1, 0.1)
        ) +
            facet_grid(cols = vars(issue)) +
            theme(
                axis.text.x = element_text(size = 13),
                strip.text.x.top = element_text(size = 12),
                legend.position = "bottom",
                plot.margin = margin(10, 10, 10, 10)
            )
    ),

    # Violence attidtudes by origin
    tar_target(dt_violence_attitudes_origin, load_data_table(file.path("data", "f6_23_masc_origmax_kon.csv"),
        names = c("issue", "gender", "origin", "proportion", "lb", "ub"),
        factor_cols = c("issue", "gender", "origin")
    ), format = "fst_dt"),
    tar_target(
        p_violence_attitudes_origin,
        bar_plot(dt_violence_attitudes_origin, "gender", "proportion",
            color_var = "origin",
            breaks = seq(0, 1, 0.1)
        ) +
            facet_grid(cols = vars(issue)) +
            theme(
                axis.text.x = element_text(size = 13),
                legend.position = "bottom",
                plot.margin = margin(10, 10, 10, 10)
            )
    ),

    # Attitudes towards integration by background
    tar_target(dt_integration_attitudes_background, load_data_table(file.path("data", "f6_24_atint_gen.csv"),
        names = c("age", "background", "proportion", "lb", "ub"),
        factor_cols = c("background")
    )[1:3, statement := "imm_keep"][
        4:6, statement := "swe_open"
    ][
        7:9, statement := "imm_adapt"
    ][
        10:12, statement := "swe_keep"
    ],
    format = "fst_dt"
    ),
    tar_target(
        p_integration_attitudes_background,
        bar_plot(dt_integration_attitudes_background,
            "statement", "proportion",
            color_var = "background",
            breaks = seq(0, 1, 0.2),
            y_limits = c(0, 0.9),
            direction = "laying",
            limits = c("SVF", "Gen 2", "Gen 1")
        ) +
            scale_x_discrete(labels = dictionary(c("imm_adapt", "imm_keep", "swe_open", "swe_keep"))) +
            theme(axis.text.y = element_text(size = 13)) +
            coord_flip()
    ),

    # Attitudes towards integration by origin
    tar_target(dt_integration_attitudes_origin, load_data_table(file.path("data", "f6_24_atint_origin.csv"),
        names = c("age", "origin", "proportion", "lb", "ub"),
        factor_cols = "origin"
    )[1:6, statement := "imm_keep"][
        7:12, statement := "swe_open"
    ][
        13:18, statement := "imm_adapt"
    ][
        19:24, statement := "swe_keep"
    ][
        origin == 0, origin := "Asien"
    ][
        origin == 1, origin := "VSÖ Afrika"
    ][
        origin == 2, origin := "MENA+"
    ][
        origin == 3, origin := "Östeuropa"
    ][
        origin == 4, origin := "NVS Europa"
    ][
        origin == 5, origin := "Svenskfödda"
    ]),
    tar_target(
        p_integration_attitudes_origin,
        bar_plot(dt_integration_attitudes_origin[, origin := forcats::fct_rev(origin)],
            "origin", "proportion",
            color_var = "origin", direction = "laying", breaks = seq(0, 1, 0.1),
            limits = c(
                "Svenskfödda",
                "NVS Europa",
                "Östeuropa",
                "MENA+",
                "VSÖ Afrika",
                "Asien"
            )
        ) +
            coord_flip() +
            facet_grid(rows = vars(forcats::fct_rev(dictionary(statement))), switch = "both") +
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                strip.text.y.left = element_text(size = 12, angle = 0),
                legend.position = "bottom"
            ) +
            guides(color = guide_legend(nrow = 2, byrow = TRUE))
    ),

    ###
    # Politisk integration

    # Discussing politics by background
    tar_target(dt_discuss_pol, 
        load_data_table(file.path("data", "f7_1_comm_gen.csv"),
            names = c("background", "frequency", "proportion", "lb", "ub"),
            factor_cols = c("background", "frequency"),
            dictionary_cols = "background"
        ), 
        format = "fst_dt"),
    tar_target(p_discuss_pol, bar_plot(dt_discuss_pol, "background", "proportion",
        color_var = "frequency",
        color_values = dot_palette(length(unique(dt_discuss_pol[, frequency])), type = "diverging")
    )),

    # Political interest over age
    tar_target(dt_political_interest_age, load_data_table(file.path("data", "f7_2_cintsc_orig_age.csv"),
        names = c("age", "background", "mean", "lb", "ub"),
        dictionary_cols = c("age", "background"),
        factor_cols = "background"
    ), format = "fst_dt"),
    tar_target(p_political_interest_age, line_plot(dt_political_interest_age[, age := as.integer(substr(age, 1, 2))], "age", "mean", group_var = "background") +
        scale_y_continuous(
            labels = c("Väldigt lite", "Lite", "En del"),
            breaks = c(0, 1, 2), limits = c(0, 2)
        ) +
        scale_x_continuous(breaks = c(15, 16, 19), labels = c("15 år", "16 år", "19 år")) +
        theme(legend.position = "bottom")),

    # Societal engagement
    tar_target(dt_soc_engage, load_data_table(file.path("data", "f7_3_polb_gen_sep_m0.csv"),
        names = c("type", "background", "value", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("type", "background")
    ), format = "fst_dt"),
    tar_target(p_soc_engage, bar_plot(dt_soc_engage, "background", "value",
        color_var = "background",
        direction = "laying", breaks = seq(0, 1, 0.1), y_limits = c(0, 0.5)
    ) +
        theme(
            strip.text.y.left = element_text(angle = 0, hjust = 1),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom"
        ) +
        facet_grid(rows = vars(type), switch = "both") + coord_flip()),

    ###
    # Adaption, framtidstro och hälsa

    # Internalising problems
    tar_target(dt_internalising, load_data_table(file.path("data", "f8_1_inter.csv"),
        names = c("group", "background", "value", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("group", "background")
    ), format = "fst_dt"),
    tar_target(p_internalising, bar_plot(dt_internalising, "background", "value",
        color_var = "background",
        direction = "laying",
        color_values = c(dot_palette(3), dot_palette(6), dot_palette(2))
    ) +
        theme(strip.text.y = element_blank(), legend.position = "none") + 
        scale_y_continuous(
            breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1.1),
            labels = c("0\n(Aldrig)", "0.2", "0.4", "0.6", "0.8", "1\n(Sällan)")
        ) +
        facet_grid(rows = vars(group), scales = "free", space = "free") + coord_flip()),

    # Internalising problems by generation, gender and age
    tar_target(dt_internalising_age, load_data_table(file.path("data", "f8_2_inter_age.csv"),
        names = c("age", "background", "mean", "lb", "ub"),
        dictionary_cols = c("background", "age"),
        factor_cols = "background"
    ), format = "fst_dt"),
    tar_target(p_internalising_age, line_plot(dt_internalising_age[, age := as.integer(substr(age, 1, 2))], "age", "mean", group_var = "background") +
        scale_y_continuous(limits = c(0, 1.6)) +
        scale_x_continuous(breaks = c(14, 15, 16, 19), labels = c("14 år", "15 år", "16 år", "19 år")) +
        theme(legend.position = "bottom")),

    # Norm breaking
    tar_target(dt_f8_3, load_data_table(file.path("data", "f8_3_norm.csv"),
        names = c("group", "background", "percentage", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("group", "background")
    ), format = "fst_dt"),
    tar_target(p_f8_3, 
        bar_plot(
            dt_f8_3[, ':='(
                        percentage = percentage / 100,
                        lb = lb / 100,
                        ub = ub /100
                        )], 
            "background", "percentage",
            color_var = "background",
            direction = "laying", y_limits = c(0, 0.3),
            color_values = c(dot_palette(3), dot_palette(6), dot_palette(2))
        ) +
        theme(strip.text.y = element_blank(), legend.position = "none") +
        facet_grid(rows = vars(group), scales = "free", space = "free") + coord_flip()),

    # Alcohol, smoking & drugs
    tar_target(dt_f8_4, load_data_table(file.path("data", "f8_4_alc_gen.csv"),
        names = c("background", "category", "proportion", "lb", "ub"),
        dictionary_cols = c("background"),
        factor_cols = c("background", "category")
    ), format = "fst_dt"),
    tar_target(p_f8_4, bar_plot(dt_f8_4, "category", "proportion",
        color_var = "category",
        breaks = seq(0, 1, 0.1),
        direction = "laying",
        y_limits = c(0, 0.41)
    ) +
        theme(
            strip.text.y.left = element_text(angle = 0),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom",
            plot.margin = margin(10, 10, 10, 10)
        ) +
        coord_flip() + facet_grid(rows = vars(background), switch = "both")),

    # Alcohol use by age
    tar_target(dt_f8_5, load_data_table(file.path("data", "f8_5_alc_age.csv"),
        names = c("age", "gender", "proportion", "lb", "ub"),
        dictionary_cols = c("age"),
        factor_cols = "gender"
    ), format = "fst_dt"),
    tar_target(p_f8_5, line_plot(dt_f8_5[, age := as.integer(substr(age, 1, 2))], "age", "proportion", group_var = "gender") +
        scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
        scale_x_continuous(breaks = c(14, 15, 16, 19), labels = c("14 år", "15 år", "16 år", "19 år")) +
        theme(legend.position = "bottom")),

    # Positive future outlook
    tar_target(dt_f8_6,
        load_data_table(file.path("data", "f8_6_gen_origmax_kon.csv"),
            names = c("category", "background", "importance", "proportion", "lb", "ub"),
            factor_cols = c("category", "background", "importance"),
            dictionary_cols = c("background")
        ),
        format = "fst_dt"
    ),
    tar_target(p_f8_6, bar_plot(reverse_sort(dt_f8_6, "background"), "background", "proportion",
        color_var = "importance",
        direction = "laying",
        color_values = rev(dot_palette(length(unique(dt_f8_6[, importance])), type = "diverging"))
    ) +
        theme(
            strip.text.y = element_blank(),
            legend.position = "bottom",
            plot.margin = margin(10, 80, 10, 10)
        ) +
        facet_grid(rows = vars(category), scales = "free", space = "free") + coord_flip()),

    # Future outlook by area and background
    tar_target(dt_futhea, load_data_table(file.path("data", "futhea_gen.csv"),
        names = c("background", "response", "proportion", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("response", "background")
    )[, question := "Ha bra hälsa"], format = "fst_dt"),
    tar_target(dt_futjon, load_data_table(file.path("data", "futjob_gen.csv"),
        names = c("background", "response", "proportion", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("response", "background")
    )[, question := "Ha ett jobb"], format = "fst_dt"),
    tar_target(dt_futmon, load_data_table(file.path("data", "futmon_gen.csv"),
        names = c("background", "response", "proportion", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("response", "background")
    )[, question := "Ha mycket pengar"], format = "fst_dt"),
    tar_target(dt_futuni, load_data_table(file.path("data", "futuni_gen.csv"),
        names = c("background", "response", "proportion", "lb", "ub"),
        dictionary_cols = "background",
        factor_cols = c("response", "background")
    )[, question := "Ha universitetsexamen"], format = "fst_dt"),
    tar_target(dt_f8_7, set_level_names(
        rbind(dt_futhea, dt_futjon, dt_futmon, dt_futuni),
        "response",
        c("Nej", "Vet inte", "Ja")
    )),
    tar_target(p_f8_7, bar_plot(reverse_sort(dt_f8_7, "background"), "background", "proportion",
        color_var = "response",
        color_values = dot_palette(length(unique(dt_f8_7[, response])), type = "diverging"),
        direction = "laying"
    ) +
        facet_grid(rows = vars(question)) +
        theme(legend.position = "bottom", strip.text.y = element_text(angle = 0)) +
        coord_flip()),

    # Life satisfaction
    tar_target(dt_f8_8,
        load_data_table(file.path("data", "f8_8_sat_gen_origmax_kon_stack.csv"),
            names = c("category", "background", "satisfaction", "proportion", "lb", "ub"),
            factor_cols = c("category", "background", "satisfaction"),
            dictionary_cols = c("background")
        ),
        format = "fst_dt"
    ),
    tar_target(p_f8_8, bar_plot(reverse_sort(dt_f8_8, "background"), "background", "proportion",
        color_var = "satisfaction",
        direction = "laying",
        color_values = dot_palette(length(unique(dt_f8_8[, satisfaction])), type = "diverging")
    ) +
        theme(
            strip.text.y = element_blank(),
            legend.position = "right",
            plot.margin = margin(10, 10, 10, 10)
        ) +
        facet_grid(rows = vars(category), scales = "free", space = "free") + coord_flip()),

    # Life satisfaction (collapsed)
    tar_target(dt_f8_8b, dt_f8_8[, satisfaction_c := fct_collapse(
        satisfaction,
        "5-7" = c("5", "6", "7"),
        "8 - 10 Mycket nöjd" = c("8", "9", "10 Mycket nöjd")
    )][,
        ':='(
            proportion_c = sum(proportion),
            lb = sum(lb),
            ub = sum(ub)
        ), 
        .(satisfaction_c, background, category)
    ]
    ),
    tar_target(p_f8_8b, bar_plot(
        reverse_sort(dt_f8_8b, "background"),
        "background", "proportion_c",
        color_var = "satisfaction_c",
        direction = "laying",
        y_limits = c(0, 1),
        color_values = dot_palette(length(unique(dt_f8_8b[, satisfaction_c])), type = "diverging")
    ) +
        theme(
            strip.text.y = element_blank(),
            legend.position = "right",
            plot.margin = margin(10, 10, 10, 10)
        ) +
        facet_grid(rows = vars(category), scales = "free", space = "free") +
        coord_flip()),


    # Self rated health
    tar_target(dt_f8_9,
        load_data_table(file.path("data", "f8_9_genhea.csv"),
            names = c("gender", "background", "health", "proportion", "lb", "ub"),
            factor_cols = c("gender", "background", "health"),
            dictionary_cols = c("background")
        ),
        format = "fst_dt"
    ),
    tar_target(p_f8_9, bar_plot(reverse_sort(dt_f8_9, "background"), "background", "proportion",
        color_var = "health", direction = "laying",
        color_values = dot_palette(length(unique(dt_f8_9[, health])), type = "diverging")
    ) +
        facet_grid(rows = vars(gender)) +
        theme(
            legend.position = "bottom",
            strip.text.y = element_text(angle = 0)
        ) +
        coord_flip()),

    ##
    # Appendix files

    # Expected highest education
    tar_target(dt_fb4_5,
        load_data_table(file.path("data", "fB4_5_effort1.csv"),
            names = c("background", "education", "proportion", "lb", "ub"),
            factor_cols = c("background", "education"),
            dictionary_cols = c("background")
        ),
        format = "fst_dt"
    ),
    tar_target(p_fb4_5, bar_plot(
        set_level_names(
            reverse_sort(dt_fb4_5, "background"),
            "education",
            c("Gymnasium eller lägre", "Högskola/universitet")
        ),
        "education", "proportion",
        color_var = "education",
        direction = "laying",
        y_limits = c(0.2, 0.8),
        color_values = dot_palette(length(unique(dt_fb4_5[, education])), type = "diverging")
    ) +
        theme(
            legend.position = "bottom",
            strip.text.y = element_text(angle = 0),
            axis.text.y.left = element_blank()
        ) +
        facet_grid(rows = vars(background)) +
        coord_flip()),

    # School effort
    tar_target(dt_eff1_gen,
        load_data_table(file.path("data", "eff1_gen.csv"),
            names = c("background", "effort", "proportion", "lb", "ub"),
            factor_cols = c("background", "effort"),
            dictionary_cols = c("background")
        ),
        format = "fst_dt"
    ),
    tar_target(p_eff1_gen, bar_plot(
        set_level_names(
            reverse_sort(dt_eff1_gen, "background"),
            "effort",
            c(
                "Instämmer inte",
                "Varken eller",
                "Instämmer",
                "Instämmer absolut"
            )
        ),
        "effort", "proportion",
        color_var = "effort",
        direction = "laying",
        y_limits = c(0, 0.6),
        color_values = dot_palette(length(unique(dt_eff1_gen[, effort])), type = "diverging")
    ) +
        theme(
            legend.position = "bottom",
            strip.text.y = element_text(angle = 0),
            axis.text.y.left = element_blank(),
            plot.margin = margin(10, 10, 10, 65)
        ) +
        facet_grid(rows = vars(background)) +
        coord_flip()),

    # Time spent doing homework
    tar_target(dt_lta11_gen,
        load_data_table(file.path("data", "lta11_gen.csv"),
            names = c("background", "time", "proportion", "lb", "ub"),
            factor_cols = c("background", "time"),
            dictionary_cols = c("background")
        ),
        format = "fst_dt"
    ),
    tar_target(p_lta11_gen, bar_plot(
        set_level_names(
            reverse_sort(dt_lta11_gen, "background"),
            "time",
            c(
                "Ingen tid",
                "<1 tim",
                "1 tim",
                "2 tim",
                ">2 tim"
            )
        ),
        "time", "proportion",
        color_var = "time",
        direction = "laying",
        y_limits = c(0, 0.6),
        color_values = dot_palette(
            length(unique(dt_lta11_gen[, time])),
            type = "diverging"
        )
    ) +
        theme(
            legend.position = "bottom",
            strip.text.y = element_text(angle = 0),
            axis.text.y.left = element_blank(),
            plot.margin = margin(10, 10, 10, 15)
        ) +
        facet_grid(rows = vars(background)) +
        coord_flip())
)
