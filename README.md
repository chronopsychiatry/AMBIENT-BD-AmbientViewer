# AmbientViewer

AmbientViewer provides tools to filter and visualise sleep data acquired by Somnofy devices (VitalThings).

## Getting started

The easiest way to use Ambient Viewer is to visit the [online app](https://shinyserver.bio.ed.ac.uk/app/07_ambientviewer-app). Visit the [wiki](https://github.com/chronopsychiatry/AmbientViewer/wiki) for more detailed instructions!

## Can I use Ambient Viewer with my data?

Ambient Viewer was initially developped to process data from Somnofy devices. However, other data types are supported, such as outputs from the GGIR package (for actigraphy), or .edf files converted with `edfs_to_csv`.

If your data is not listed here, feel free to [open an issue](https://github.com/chronopsychiatry/AmbientViewer/issues) (preferably with a sample dataset) for it to be supported by Ambient Viewer.

## Running Ambient Viewer locally

If you wish to run the app locally, or to use the R package, please follow the [installation instructions](https://github.com/chronopsychiatry/AmbientViewer/wiki/Installation) and [how to get started](https://github.com/chronopsychiatry/AmbientViewer/wiki/Getting-started).

Descriptions of functions from the Ambient Viewer R package can be found in the [PDF manual](https://github.com/chronopsychiatry/AmbientViewer/tree/main/Package_manuals). Select the package version that corresponds to the one you have installed. If you are unsure, you can check your current version of Ambient Viewer by running `packageVersion("AmbientViewer")` in R.

The [changelog](https://github.com/chronopsychiatry/AmbientViewer/blob/main/CHANGELOG.md) contains information about changes made in each version. Generally, it is preferable to run the latest version of the package, as each version will contain bug fixes and improvements. You can update your installed version by running `devtools::install_github("Chronopsychiatry/AmbientViewer", force = TRUE)` in R.

## Bugs and suggestions

To report a bug or request a new feature, [open a new issue](https://github.com/chronopsychiatry/AmbientViewer/issues).

To make suggestions, request new features or discuss how to use the app or package, [start a new discussion](https://github.com/chronopsychiatry/AmbientViewer/discussions).

## Contact

Maintainer: [daniel.thedie@ed.ac.uk](mailto:daniel.thedie@ed.ac.uk)

Ambient Viewer is developed by the [BioRDM team](https://biology.ed.ac.uk/research/facilities/research-data-management) at the University of Edinburgh, as part of the [Ambient-BD project](https://www.ambientbd.com/).
