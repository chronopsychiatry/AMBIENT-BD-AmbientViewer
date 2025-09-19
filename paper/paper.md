---
title: 'Ambient Viewer: an online tool and R package for sleep data visualisation'
tags:
  - R
  - circadian research
  - sleep
  - somnofy
authors:
  - name: Daniel Thedie
    orcid: 0000-0002-1352-7245
    affiliation: 1
  - name: Andrew J. Millar
    orcid: 0000-0003-1756-3654
    corresponding: true
    affiliation: 1
  - name: Daniel Smith
    orcid: 0000-0002-2267-1951
    affiliation: 3
affiliations:
 - name: The University of Edinburgh School of Biological Sciences, Edinburgh, Scotland, UK
   index: 1
 - name: The University of Edinburgh Division of Psychiatry, Edinburgh, Scotland, UK
   index: 2
date: 17 September 2025
bibliography: paper.bib
---

# Summary


# Statement of need

Radar technology has recently emerged as a useful tool for longitudinal sleep monitoring for wellbeing and research. It has been used in the Ambient-BD study (cite), where Somnofy devices (VitalThings) were used to monitor the sleep of 180 participants over 18 months. Somnofy is a promising tool for research applications, as it allows long-term, non-intrusive sleep monitoring, with minimal maintenance. This can be harnessed to pick up long-term trends in sleep and circadian rhythm, and monitor related health outcomes. However, due to the novelty of this technology, few specialised tools are currently available for the analysis of radar-derived sleep data, as is produced by Somnofy. Ambient Viewer was developed as part of the Ambient-BD study to enable researchers to:
- Explore Somnofy sleep data, regardless of their familiarity with programming languages
- Apply thresholds on key variables (such as time spent in bed) to remove spurious sleep sessions, for example caused by pets lying on the bed
- Generate attractive visualisations that can be used in research outputs during and after the project
- Produce accessible sleep summaries to be shared with study participants
- Create automated workflows to quickly produce the outputs listed above for a large number of participants

To achieve these aims, Ambient Viewer was developped as an R package and an R shiny app, which can be accessed directly online, or run locally.

# Structure of the Somnofy data

During the Ambient-BD project, the raw radar data produced by Somnofy devices was processed internally by VitalThings to extract information such as sleep onset and wakeup time, as well as classifying sleep stages (awake, light sleep, REM, deep sleep). The processed data was then made available to the researchers via an API, in the form of two separate .csv files:
- Sessions file: one row per sleep session, with data including time at sleep onset, time at wakeup, time spent in bed, and aggregate values such as the average temperature during the session
- Epochs file: timestamped sleep data with 30-second resolution, including classification of sleep stages. A session ID is indicated for each timepoint, so epoch data can be linked with session data

# Main functionalities

## Interface

Ambient Viewer can be used through a graphical interface (running online or locally), or as an R package.

The Ambient Viewer graphical interface is conceived as a dashboard, where the different information and menus are easily reachable. It contains five main areas:
- Data input
- Data filtering
- Data export
- Tables
- Figures

The application is reactive, allowing for direct feedback on the effect of data filters.

Main functionalities and usefulness compared to app.

## Data filtering



## Using Ambient Viewer with sleep data from different sources

Explain GGIR + process to use differently formatted data

# Acknowledgements

Chronopsychiatry group

# Funding

Ambient-BD grant

# References
