##### 

<br>

    library(tidyr)
    library(ggplot2)
    library(knitr)
    library(DT)
    library(stringr)
    library(xtable)
    library(hrbrthemes)
    library(dplyr)
    library(forcats)
    library(finalfit)
    library(dplyr)
    library(stringr)
    library(janitor)
    # library(sf)
    # library(ggsn)
    # library(htmltools)
    library(lme4)
    library(extrafont)
    library(cowplot)
    library(knitr)
    library(parameters)

    # Load fonts
    extrafont::loadfonts(quiet = TRUE)

    bats_res = readRDS("data/Ethiopia_bat-info_test-summary.RDS")
    bats_res$SiteName = trimws(gsub("-\\<Bat\\>|\\<Bat\\>", "", bats_res$SiteName))
    bats_res$ConcurrentSamplingSite = bats_res$SiteName

    tests = readRDS("data/Ethiopia_bat-test-info.RDS")
    tests$SiteName = trimws(gsub("-\\<Bat\\>|\\<Bat\\>", "", tests$SiteName))

    events = readRDS("data/Ethiopia_bat-sampling-events.RDS")

    names(bats_res)

    df = bats_res
    df$Family = str_to_title(df$Family)
    tests = left_join(tests, df[ , c("AnimalID", "Family")], by = "AnimalID")

##### Check that all bats were tested the same (unique test combinations):

    datatable(xtable(table(df$tests_done)), options = list(dom = 't'))

![](../results/figs/test-summary/unnamed-chunk-2-1.png)

<br><br>

Number of bats testing positive
-------------------------------

Number of bats tested = 589

Number of bats testing positive = 99

Percent bats testing positive = 16.8081494

<br><br>

Species of bats tested
----------------------

    datatable(xtable(table(df$ScientificName, df$Family)))

![](../results/figs/test-summary/unnamed-chunk-3-1.png)

<br><br>

Sampling events
---------------

### Unique events

    # lapply(split(events, events$site_name), function(x) sort(unique(x$event_date)))
    lapply(split(df, df$SiteName), function(x) sort(unique(x$EventDate)))

    ## $`Awash-Metehara`
    ## [1] "2016-07-16" "2016-08-16" "2017-08-05" "2017-09-02" "2018-03-30"
    ## [6] "2018-05-16" "2018-08-02" "2018-08-30"
    ## 
    ## $Bati
    ## [1] "2018-05-31" "2018-06-22" "2018-08-22" "2018-10-15"

    lapply(split(df, df$SiteName), function(x) table(x$EventDate, x$SeasonModelled, x$ScientificName, x$ConcurrentSamplingSite))

    ## $`Awash-Metehara`
    ## , ,  = Chaerephon pumilus,  = Awash-Metehara
    ## 
    ##             
    ##              Dry Wet
    ##   2016-07-16   0  44
    ##   2016-08-16   0  16
    ##   2017-08-05   0  47
    ##   2017-09-02   0  41
    ##   2018-03-30   0  42
    ##   2018-05-16  38   0
    ##   2018-08-02   0   0
    ##   2018-08-30   0   0
    ## 
    ## , ,  = Rhinopoma hardwickii,  = Awash-Metehara
    ## 
    ##             
    ##              Dry Wet
    ##   2016-07-16   0   9
    ##   2016-08-16   0  24
    ##   2017-08-05   0   0
    ##   2017-09-02   0  21
    ##   2018-03-30   0  10
    ##   2018-05-16  10   0
    ##   2018-08-02   0  52
    ##   2018-08-30   0  48
    ## 
    ## 
    ## $Bati
    ## , ,  = Mops midas,  = Bati
    ## 
    ##             
    ##              Dry Wet
    ##   2018-05-31  35   0
    ##   2018-06-22   0  45
    ##   2018-08-22   0  73
    ##   2018-10-15  27   0
    ## 
    ## , ,  = Neoromicia cf. somalica,  = Bati
    ## 
    ##             
    ##              Dry Wet
    ##   2018-05-31   7   0
    ##   2018-06-22   0   0
    ##   2018-08-22   0   0
    ##   2018-10-15   0   0

<br><br>

### Figure 1: Sampling effort by event

<!-- ![](../figures/Figure 2 Event-timeline_V2.png) -->
<br><br>

General virus information
-------------------------

### Numbers of viruses detected

    datatable(xtable(table(unlist(strsplit(df$pos_virus[ df$is_positive ], "; ")))))

![](../results/figs/test-summary/unnamed-chunk-6-1.png)

<br><br>

Specific virus information
--------------------------

### Table 1. Positive bats frequency by family, species and site name

    df_ani =

        tests %>%
        select(SiteName, ScientificName, AnimalID) %>%
        group_by(SiteName, ScientificName) %>%
        summarize(total_bats = length(unique(AnimalID))) %>%
        as.data.frame() %>%
        select(ScientificName, total_bats) %>%
        adorn_totals("row")



    df_pos_ani =

        tests %>%
        select(SiteName, Family, ScientificName, AnimalID, VirusGroup) %>%
        filter( !is.na(VirusGroup)) %>%
        group_by(SiteName, Family, ScientificName) %>%
        summarize(bats = length(unique(AnimalID))) %>%
        as.data.frame() %>%
        adorn_totals("row")



    df_pos_ani$bats = paste0(df_pos_ani$bats, "/",
                                      df_ani$total_bats)


    df_spmn = 

        tests %>%
        select(SiteName, ScientificName, SpecimenType, SpecimenID) %>%
        mutate(SpecimenType = paste0("n_",
                                     gsub(" ", "_", SpecimenType))) %>%
        group_by(SiteName, ScientificName, SpecimenType) %>%
        summarize(total = length(unique(SpecimenID))) %>%
        as.data.frame() %>%
        spread(SpecimenType, total) %>%
        adorn_totals("row") %>%
        select(-SiteName)

    df_pos_spmn =

        tests %>%
        select(SiteName, ScientificName, SpecimenType, SpecimenID, VirusGroup) %>%
        filter( !is.na(VirusGroup)) %>%
        mutate(SpecimenType = gsub(" ", "_", SpecimenType)) %>%
        group_by(SiteName, ScientificName, SpecimenType) %>%
        summarize(pos = length(unique(SpecimenID))) %>%
        as.data.frame() %>%
        spread(SpecimenType, pos) %>%
        mutate_all(~replace_na(., 0)) %>%
        adorn_totals("row") %>%
        select( -SiteName)

    df_pos_spmn$oral_swab = paste0(df_pos_spmn$oral_swab, "/",
                                   df_spmn$n_oral_swab)

    df_pos_spmn$rectal_swab = paste0(df_pos_spmn$rectal_swab, "/",
                                     df_spmn$n_rectal_swab)

    pos_viruses =

        tests %>%
        select(ScientificName, VirusGroup, ViralFamily) %>%
        filter( !is.na(VirusGroup)) %>%
        group_by(ScientificName) %>%
        summarize(
            ViralFamily = paste0(sort(unique(ViralFamily)), collapse = "; "),
            Viruses = paste0(sort(unique(VirusGroup)), collapse = "; "))

    bat_virus_info = left_join(df_pos_ani, df_pos_spmn, by = "ScientificName")
    bat_virus_info = left_join(bat_virus_info, pos_viruses, by = "ScientificName")

    datatable(bat_virus_info)

![](../results/figs/test-summary/unnamed-chunk-7-1.png)

<br><br>

### Coinfections

    df %>%
        filter( is_positive & pos_virus_n > 1 ) %>%
        select(SiteName, ScientificName, pos_virus) %>%
        group_by(SiteName, ScientificName, pos_virus) %>%
        summarize(n_bats = n()) %>%
        datatable()

![](../results/figs/test-summary/unnamed-chunk-8-1.png)

<br><br>

### Figure 2. Virus plot

    bat_virus_n = df %>%
        select(pos_virus, ScientificName, AnimalID) %>%
        group_by(pos_virus, ScientificName) %>%
        tally()
        

    virus_plot =

        df %>%
        select(pos_virus, pos_virus_n, pos_virus_family,
               ScientificName, SeasonModelled) %>%
        drop_na() %>%
        mutate(pos_virus = gsub("; ", "\nand ", pos_virus)) %>%
        group_by(pos_virus, pos_virus_n, pos_virus_family,
                 ScientificName, SeasonModelled) %>%
        summarize(n = n()) %>%
        arrange(pos_virus_family, desc(n)) %>%
        as.data.frame %>%
        mutate(pos_virus = factor(pos_virus, unique(pos_virus)),
               pos_virus = fct_rev(pos_virus),
               ScientificName = factor(ScientificName,
                                       levels = c(
                                       "Rhinopoma hardwickii",
                                       "Chaerephon pumilus",
                                       "Mops midas", 
                                       "Neoromicia cf. somalica"
                                       ))) %>%
        # ggplot( aes(x=pos_virus, y=n, fill = as.factor(pos_virus_n), label = n) ) +
        ggplot(aes(x = reorder(pos_virus, n), y=n, fill = ScientificName, label = n) ) +
        facet_wrap( ~ SeasonModelled ) +
        scale_fill_manual(values = c("#CC6677", "#44AA99", "#DDCC77", "#AA4499"),
                          name = "Bat species and\nsampling interface") +
        geom_bar(stat="identity") +
        ylim(0, 80) +
        # geom_text( size = 3, nudge_y = 4) +
        coord_flip() +
        theme_ipsum(base_size = 10) +
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position="right",
          legend.text = element_text(face = "italic")
        ) +
        xlab("") +
        ylab("Number of bats") +
        labs(title = "Viruses detected in bats")


    cowplot::save_plot("results/figs/virus-plot.pdf",
                       virus_plot, base_height = 5)

    virus_plot

![](../results/figs/test-summary/unnamed-chunk-9-1.png)

<!-- ![](../figures/Figure 3 Virus-plot.png) -->
<br><br>

Virus detection by specimen type
--------------------------------

### Breakdown of positive specimen types (at individual bat level)

Rectal swabs were the most common specimen type to yield a positive
virus sequence (92/99 bats), including 27 bats from whom sequences were
confirmed in both rectal and oral swabs. In addition, seven bats had
viruses detected only via their oral swabs.

    df %>%
        filter(is_positive) %>%
        group_by(pos_specimen_type) %>%
        tally() %>%
        adorn_totals("row") %>%
        datatable(options = list(dom = 't'))

![](../results/figs/test-summary/unnamed-chunk-10-1.png)

<br><br>

### Positive specimen types for bats with &gt; 1 virus detected

    ids = df$AnimalID[ which(df$pos_virus_n > 1) ]
    multi_bats = unique(
                     tests[ tests$AnimalID %in% ids & !is.na(tests$VirusGroup),
                           c("AnimalID", "SpecimenType", "VirusGroup")]
                 )


    tbls = vector("list", 3)
    for(i in seq_along(ids)) {
        tbls[[ i ]] = 
            multi_bats %>%
            filter(AnimalID %in% ids[ i ]) %>%
            datatable(options = list(dom = 't'))
    }

    tbls[[1]]

![](../results/figs/test-summary/unnamed-chunk-11-1.png)

    tbls[[2]]

![](../results/figs/test-summary/unnamed-chunk-11-2.png)

    tbls[[3]]

![](../results/figs/test-summary/unnamed-chunk-11-3.png)

<br><br>

### Virus group by specimen type (at individual bat level)

    df %>%
        filter(is_positive) %>%
        select(pos_virus, pos_specimen_type) %>%
        group_by(pos_virus, pos_specimen_type) %>%
        tally() %>%
        as_data_frame() %>%
        spread(pos_specimen_type, n) %>%
        adorn_totals(c("row", "col")) %>%
        datatable(options = list(dom = 't'))

![](../results/figs/test-summary/unnamed-chunk-12-1.png)

<br><br>

Table 2. Fisher's exact tests
-----------------------------

### Table 2. Age, Sex, Season, species, specimen type

Note from Julius' paper: Fisher’s exact test was used to examine the
association of viral positivity with age and season using STATA 13.0
software. The level of significance was set at P 0.05 (Raymond and
Rousset 1995).

#### Crosstable

    df$AgeClass = word(df$AgeClass, 1)

    # table1( ~ is_positive | Sex, df)

    # Crosstable 
    dependent = "is_positive"
    explanatory = c("Sex",
                    "AgeClass",
                    "SeasonModelled",
                    "ScientificName")

    df %>%
        summary_factorlist(dependent, explanatory, 
                           p = TRUE,
                           add_dependent_label = TRUE,
                           p_cat = "fisher" # Fisher's Exact test 
                           ) -> t1
    knitr::kable(t1, align=c("l", "l", "r", "r", "r"))

<table>
<thead>
<tr class="header">
<th align="left">Dependent: is_positive</th>
<th></th>
<th align="right">FALSE</th>
<th align="right">TRUE</th>
<th align="right">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sex</td>
<td>female</td>
<td align="right">343 (70.0)</td>
<td align="right">66 (66.7)</td>
<td align="right">0.550</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>male</td>
<td align="right">147 (30.0)</td>
<td align="right">33 (33.3)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">AgeClass</td>
<td>adult</td>
<td align="right">451 (92.0)</td>
<td align="right">82 (82.8)</td>
<td align="right">0.008</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>subadult</td>
<td align="right">39 (8.0)</td>
<td align="right">17 (17.2)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">SeasonModelled</td>
<td>Dry</td>
<td align="right">111 (22.7)</td>
<td align="right">6 (6.1)</td>
<td align="right">&lt;0.001</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>Wet</td>
<td align="right">379 (77.3)</td>
<td align="right">93 (93.9)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">ScientificName</td>
<td>Chaerephon pumilus</td>
<td align="right">214 (43.7)</td>
<td align="right">14 (14.1)</td>
<td align="right">&lt;0.001</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>Mops midas</td>
<td align="right">168 (34.3)</td>
<td align="right">12 (12.1)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left"></td>
<td>Neoromicia cf. somalica</td>
<td align="right">6 (1.2)</td>
<td align="right">1 (1.0)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td>Rhinopoma hardwickii</td>
<td align="right">102 (20.8)</td>
<td align="right">72 (72.7)</td>
<td align="right"></td>
</tr>
</tbody>
</table>

    df_spm = df[ , c("AnimalID", "is_positive", "pos_specimen_type")]
    df_spm = rbind(df_spm, df_spm)
    df_spm$specimen = rep(c("oral swab", "rectal swab"), each = nrow(df))

    df_spm$specimen_pos = mapply(grepl, df_spm$specimen, df_spm$pos_specimen_type)


    df %>%
        mutate(interface = ifelse(ScientificName %in% "Rhinopoma hardwickii", "Cave",
                                  "Dwelling")) %>%
        summary_factorlist("is_positive", "interface", 
                           p = TRUE,
                           add_dependent_label=TRUE,
                           p_cat = "fisher" # Fisher's Exact test 
                           ) -> t2
    knitr::kable(t2, align=c("l", "l", "r", "r", "r"))

<table>
<thead>
<tr class="header">
<th align="left">Dependent: is_positive</th>
<th></th>
<th align="right">FALSE</th>
<th align="right">TRUE</th>
<th align="right">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">interface</td>
<td>Cave</td>
<td align="right">102 (20.8)</td>
<td align="right">72 (72.7)</td>
<td align="right">&lt;0.001</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>Dwelling</td>
<td align="right">388 (79.2)</td>
<td align="right">27 (27.3)</td>
<td align="right"></td>
</tr>
</tbody>
</table>

    df_spm %>%
        summary_factorlist("specimen_pos", "specimen", 
                           p = TRUE,
                           add_dependent_label=TRUE,
                           p_cat = "fisher" # Fisher's Exact test 
                           ) -> t3
    knitr::kable(t3, align=c("l", "l", "r", "r", "r"))

<table>
<thead>
<tr class="header">
<th align="left">Dependent: specimen_pos</th>
<th></th>
<th align="right">FALSE</th>
<th align="right">TRUE</th>
<th align="right">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">specimen</td>
<td>oral swab</td>
<td align="right">555 (52.8)</td>
<td align="right">34 (27.0)</td>
<td align="right">&lt;0.001</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>rectal swab</td>
<td align="right">497 (47.2)</td>
<td align="right">92 (73.0)</td>
<td align="right"></td>
</tr>
</tbody>
</table>

    # df_spm %>%
    #     ggplot(aes(x = specimen, y = specimen_pos)) +
    #     geom_bar(stat="identity", color="black", 
    #            position=position_dodge())

<br><br>

### Data

    df$AgeClass = word(df$AgeClass, 1)

    df_summary = df %>%
        group_by(ScientificName, SiteName, SeasonModelled, Sex, AgeClass, is_positive) %>%
        summarize(n = n()) %>%
        mutate(is_positive = ifelse(is_positive, "Positive", "Negative")) %>%
        spread(key = is_positive, value = n) %>%
        mutate_at(vars(Negative, Positive), replace_na, 0) %>%
        mutate(Total = Negative + Positive) %>%
        as.data.frame %>%
        mutate_at(vars(AgeClass, ScientificName), as.factor) %>%
        mutate(neg_percent = round(Negative/Total * 100, 2),
               pos_percent = round(Positive/Total * 100, 2))
        

    DT::datatable(df_summary, extensions = 'Buttons', filter = 'top', fillContainer = FALSE)

![](../results/figs/test-summary/unnamed-chunk-14-1.png)

<br><bbr>

Exploring age/sex/season by species
-----------------------------------

### Rhinopoma

#### Crosstable

    # Crosstable 
    dependent = "is_positive"
    explanatory = c("Sex",
                    "AgeClass",
                    "SeasonModelled")

    df %>%
        filter(ScientificName %in% "Rhinopoma hardwickii") %>%
        summary_factorlist(dependent, explanatory, 
                           p=TRUE,
                           add_dependent_label=TRUE,
                           p_cat = "fisher" # Fisher's Exact test 
                           ) %>%
        knitr::kable(align=c("l", "l", "r", "r", "r"))

<table>
<thead>
<tr class="header">
<th align="left">Dependent: is_positive</th>
<th></th>
<th align="right">FALSE</th>
<th align="right">TRUE</th>
<th align="right">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sex</td>
<td>female</td>
<td align="right">77 (75.5)</td>
<td align="right">49 (68.1)</td>
<td align="right">0.305</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>male</td>
<td align="right">25 (24.5)</td>
<td align="right">23 (31.9)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">AgeClass</td>
<td>adult</td>
<td align="right">83 (81.4)</td>
<td align="right">59 (81.9)</td>
<td align="right">1.000</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>subadult</td>
<td align="right">19 (18.6)</td>
<td align="right">13 (18.1)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">SeasonModelled</td>
<td>Dry</td>
<td align="right">10 (9.8)</td>
<td align="right"></td>
<td align="right">0.006</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>Wet</td>
<td align="right">92 (90.2)</td>
<td align="right">72 (100.0)</td>
<td align="right"></td>
</tr>
</tbody>
</table>

<br><br>

### Chaerephon

#### Crosstable

    # Crosstable 
    dependent = "is_positive"
    explanatory = c(
                      "Sex",
                      "AgeClass",
                      "SeasonModelled")

    df %>%
        filter(ScientificName %in% "Chaerephon pumilus") %>%
        summary_factorlist(dependent, explanatory, 
                           p=TRUE,
                           add_dependent_label=TRUE,
                           p_cat = "fisher" # Fisher's Exact test 
                           ) %>%
        knitr::kable(align=c("l", "l", "r", "r", "r"))

<table>
<thead>
<tr class="header">
<th align="left">Dependent: is_positive</th>
<th></th>
<th align="right">FALSE</th>
<th align="right">TRUE</th>
<th align="right">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sex</td>
<td>female</td>
<td align="right">133 (62.1)</td>
<td align="right">8 (57.1)</td>
<td align="right">0.779</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>male</td>
<td align="right">81 (37.9)</td>
<td align="right">6 (42.9)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">AgeClass</td>
<td>adult</td>
<td align="right">194 (90.7)</td>
<td align="right">10 (71.4)</td>
<td align="right">0.046</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>subadult</td>
<td align="right">20 (9.3)</td>
<td align="right">4 (28.6)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">SeasonModelled</td>
<td>Dry</td>
<td align="right">36 (16.8)</td>
<td align="right">2 (14.3)</td>
<td align="right">1.000</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>Wet</td>
<td align="right">178 (83.2)</td>
<td align="right">12 (85.7)</td>
<td align="right"></td>
</tr>
</tbody>
</table>

<br><br>

### Mops

#### Crosstable

    # Crosstable 
    dependent = "is_positive"
    explanatory = c(
                      "Sex",
                      # "AgeClass", # there are no subadult for Mops
                      "SeasonModelled")

    df %>%
        filter(ScientificName %in% "Mops midas") %>%
        summary_factorlist(dependent, explanatory, 
                           p=TRUE,
                           add_dependent_label=TRUE,
                           p_cat = "fisher" # Fisher's Exact test 
                           ) %>%
        knitr::kable(align=c("l", "l", "r", "r", "r"))

<table>
<thead>
<tr class="header">
<th align="left">Dependent: is_positive</th>
<th></th>
<th align="right">FALSE</th>
<th align="right">TRUE</th>
<th align="right">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sex</td>
<td>female</td>
<td align="right">128 (76.2)</td>
<td align="right">8 (66.7)</td>
<td align="right">0.491</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>male</td>
<td align="right">40 (23.8)</td>
<td align="right">4 (33.3)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">SeasonModelled</td>
<td>Dry</td>
<td align="right">59 (35.1)</td>
<td align="right">3 (25.0)</td>
<td align="right">0.549</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>Wet</td>
<td align="right">109 (64.9)</td>
<td align="right">9 (75.0)</td>
<td align="right"></td>
</tr>
</tbody>
</table>

<br><br>

### Neoromicia cf. somalica

#### Crosstable

    # Crosstable 
    dependent = "is_positive"
    explanatory = c(
                      "Sex"
                      # "AgeClass"
                      # "SeasonModelled"
                  )

    df %>%
        filter(ScientificName %in% "Neoromicia cf. somalica") %>%
        summary_factorlist(dependent, explanatory, 
                           p=TRUE,
                           add_dependent_label=TRUE,
                           p_cat = "fisher" # Fisher's Exact test 
                           ) %>%
        knitr::kable(align=c("l", "l", "r", "r", "r"))

<table>
<thead>
<tr class="header">
<th align="left">Dependent: is_positive</th>
<th></th>
<th align="right">FALSE</th>
<th align="right">TRUE</th>
<th align="right">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sex</td>
<td>female</td>
<td align="right">5 (83.3)</td>
<td align="right">1 (100.0)</td>
<td align="right">1.000</td>
</tr>
<tr class="even">
<td align="left"></td>
<td>male</td>
<td align="right">1 (16.7)</td>
<td align="right"></td>
<td align="right"></td>
</tr>
</tbody>
</table>

<br>

------------------------------------------------------------------------

<br>

Multivariable logistic regression analyses
------------------------------------------

#### For *Rhinopoma hardwickii*, *Chaerephon pumilus*, and *Mops midas*

    df_filter = df %>%
        filter(! ScientificName %in% c("Neoromicia cf. somalica", "Mops midas"))

    m1 = glm(is_positive ~ Sex + AgeClass + SeasonModelled + ScientificName,
               data = df_filter, family = binomial)

    # summary(m1)
    print_md(model_parameters(m1, exponentiate = TRUE))

<table>
<thead>
<tr class="header">
<th align="left">Parameter</th>
<th align="center">Odds Ratio</th>
<th align="center">SE</th>
<th align="center">95% CI</th>
<th align="center">z</th>
<th align="center">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">(Intercept)</td>
<td align="center">0.02</td>
<td align="center">0.01</td>
<td align="center">(2.30e-03, 0.06)</td>
<td align="center">-5.37</td>
<td align="center">&lt; .001</td>
</tr>
<tr class="even">
<td align="left">Sex (male)</td>
<td align="center">1.31</td>
<td align="center">0.38</td>
<td align="center">(0.73, 2.33)</td>
<td align="center">0.92</td>
<td align="center">0.359</td>
</tr>
<tr class="odd">
<td align="left">AgeClass (subadult)</td>
<td align="center">1.23</td>
<td align="center">0.44</td>
<td align="center">(0.60, 2.45)</td>
<td align="center">0.58</td>
<td align="center">0.562</td>
</tr>
<tr class="even">
<td align="left">SeasonModelled (Wet)</td>
<td align="center">4.33</td>
<td align="center">3.30</td>
<td align="center">(1.20, 27.83)</td>
<td align="center">1.92</td>
<td align="center">0.055</td>
</tr>
<tr class="odd">
<td align="left">ScientificName (Rhinopoma hardwickii)</td>
<td align="center">10.22</td>
<td align="center">3.30</td>
<td align="center">(5.59, 19.93)</td>
<td align="center">7.21</td>
<td align="center">&lt; .001</td>
</tr>
</tbody>
</table>

    # 
    # m2 = glm(is_positive ~ Sex + SeasonModelled + ScientificName,
    #            data = df_filter, family = binomial)
    # 
    # # summary(m2)
    # print_md(model_parameters(m2, exponentiate = TRUE))


    df_filter = df %>%
        filter(! ScientificName %in% c("Neoromicia cf. somalica"))


    m3 = glm(is_positive ~ Sex + SeasonModelled + ScientificName,
               data = df_filter, family = binomial)

    # summary(m3)
    print_md(model_parameters(m3, exponentiate = TRUE))

<table>
<thead>
<tr class="header">
<th align="left">Parameter</th>
<th align="center">Odds Ratio</th>
<th align="center">SE</th>
<th align="center">95% CI</th>
<th align="center">z</th>
<th align="center">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">(Intercept)</td>
<td align="center">0.02</td>
<td align="center">0.01</td>
<td align="center">(7.21e-03, 0.06)</td>
<td align="center">-6.85</td>
<td align="center">&lt; .001</td>
</tr>
<tr class="even">
<td align="left">Sex (male)</td>
<td align="center">1.35</td>
<td align="center">0.36</td>
<td align="center">(0.80, 2.27)</td>
<td align="center">1.13</td>
<td align="center">0.260</td>
</tr>
<tr class="odd">
<td align="left">SeasonModelled (Wet)</td>
<td align="center">2.73</td>
<td align="center">1.37</td>
<td align="center">(1.11, 8.23)</td>
<td align="center">2.01</td>
<td align="center">0.044</td>
</tr>
<tr class="even">
<td align="left">ScientificName (Mops midas)</td>
<td align="center">1.31</td>
<td align="center">0.54</td>
<td align="center">(0.57, 2.95)</td>
<td align="center">0.66</td>
<td align="center">0.512</td>
</tr>
<tr class="odd">
<td align="left">ScientificName (Rhinopoma hardwickii)</td>
<td align="center">10.53</td>
<td align="center">3.38</td>
<td align="center">(5.78, 20.49)</td>
<td align="center">7.34</td>
<td align="center">&lt; .001</td>
</tr>
</tbody>
</table>

    glm(is_positive ~ Sex + AgeClass + SeasonModelled + ScientificName,
        data = df_filter, family = binomial) %>%
        model_parameters(exponentiate = TRUE) %>%
        print_md()

<table>
<thead>
<tr class="header">
<th align="left">Parameter</th>
<th align="center">Odds Ratio</th>
<th align="center">SE</th>
<th align="center">95% CI</th>
<th align="center">z</th>
<th align="center">p</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">(Intercept)</td>
<td align="center">0.02</td>
<td align="center">0.01</td>
<td align="center">(7.15e-03, 0.06)</td>
<td align="center">-6.86</td>
<td align="center">&lt; .001</td>
</tr>
<tr class="even">
<td align="left">Sex (male)</td>
<td align="center">1.35</td>
<td align="center">0.36</td>
<td align="center">(0.80, 2.28)</td>
<td align="center">1.13</td>
<td align="center">0.259</td>
</tr>
<tr class="odd">
<td align="left">AgeClass (subadult)</td>
<td align="center">1.26</td>
<td align="center">0.45</td>
<td align="center">(0.62, 2.50)</td>
<td align="center">0.64</td>
<td align="center">0.522</td>
</tr>
<tr class="even">
<td align="left">SeasonModelled (Wet)</td>
<td align="center">2.67</td>
<td align="center">1.34</td>
<td align="center">(1.09, 8.06)</td>
<td align="center">1.96</td>
<td align="center">0.050</td>
</tr>
<tr class="odd">
<td align="left">ScientificName (Mops midas)</td>
<td align="center">1.35</td>
<td align="center">0.56</td>
<td align="center">(0.59, 3.05)</td>
<td align="center">0.71</td>
<td align="center">0.475</td>
</tr>
<tr class="even">
<td align="left">ScientificName (Rhinopoma hardwickii)</td>
<td align="center">10.39</td>
<td align="center">3.34</td>
<td align="center">(5.69, 20.23)</td>
<td align="center">7.28</td>
<td align="center">&lt; .001</td>
</tr>
</tbody>
</table>
