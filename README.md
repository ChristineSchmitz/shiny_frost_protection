# Shiny App for a decision model for the choice of frost protection measures

## Background
We developed this Shiny App to make our [decision support model for the choice of frost protection measures] (https://doi.org/10.5281/zenodo.11473204) for apple orchards usable for interested people in the field of horticulture. 
A detailed description of the model is published in the research article [Model-based decision support for the choice of active spring frost protection measures in apple production](https://doi.org/10.1016/j.agsy.2024.104255) (Schmitz et al., 2025)[^1]
In short, the model was developed to show how active frost protection measures differ in their economic performance and yield protection efficacy. The frost protection measures in the comparison are overhead and below-canopy irrigation, stationary and mobile wind machines, pellet heaters, candles as well as tractor-mounted and portable gas heaters. 

## Technical requirements
To run the app locally on your computer, you need R and the packages shiny[^2], shinyWidgets[^3], datamods[^4], bslib[^5], reactable[^6], decisionSupport[^7] and tidyverse[^8].

## Repository content
[`www`](https://github.com/ChristineSchmitz/shiny_frost_protection/tree/main/www)is folder where all pictures needed in the App are stored. 

[`shiny_frost_protection.Rproj`](https://github.com/ChristineSchmitz/shiny_frost_protection/blob/main/shiny_frost_protection.Rproj) is the R project file for running the app

[`app.R`](https://github.com/ChristineSchmitz/shiny_frost_protection/blob/main/app.R) is the main file with the Shiny App and contains the UI and the server part. 

[`Frost_protection_input_apple_shiny.csv`](https://github.com/ChristineSchmitz/shiny_frost_protection/blob/main/Frost_protection_input_apple_shiny.csv) contains the initial input parameters

[`frost_prot_function.R`](https://github.com/ChristineSchmitz/shiny_frost_protection/blob/main/frost_prot_function.R) contains the decision function to compare ative frost protection measures in apple production with the apple production without frost protection. 

[`manifest.json`](https://github.com/ChristineSchmitz/shiny_frost_protection/blob/main/manifest.json) lists all packages and version, which we used during the app development. This is needed in case you want to deploy the app on a Posit connect server. 

## Acknowledgments
Thanks to [Lars Zimmermann](https://github.com/Lars-Zimmermann), [Katja Schiffers](https://github.com/katjaschiffers), [Cory Whitney](https://github.com/CWWhitney), Martin Balmer and [Eike Luedeling](https://github.com/eikeluedeling) for their contribution in the development of the decision model. Furthermore, i thank all experts who contributed their knowledge to the workshop and model building

## Funding
This work was part of the [Experimentierfeld Südwest](https://ef-sw.de/) funded by the German Federal Ministry of Food and Agriculture [grant number: 28DE111B22].

## References
[^1]Schmitz, C.,Zimmermann, L.,Schiffers, K.,Whitney, C.,Balmer, M.,Luedeling, E.,2025. Model-based decision support forthechoice ofactive spring frostprotection measures inapple production. Agric. Syst.224,104255. https://doi.org/10.1016/j. agsy.2024.104255.
[^2]: Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2025).
  _shiny: Web Application Framework for R_. R package version 1.11.1,
  <https://CRAN.R-project.org/package=shiny>.
[^3]: Perrier V, Meyer F, Granjon D (2025). _shinyWidgets: Custom Inputs Widgets for Shiny_. R package version
  0.9.0, <https://CRAN.R-project.org/package=shinyWidgets>.
[^4]: Perrier V, Meyer F, Goumri S, Abeer Z (2024). _datamods: Modules to Import and Manipulate Data in 'Shiny'_. R
  package version 1.5.3, <https://CRAN.R-project.org/package=datamods>.
[^5]: Sievert C, Cheng J, Aden-Buie G (2025). _bslib: Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'_.
  R package version 0.9.0, <https://CRAN.R-project.org/package=bslib>.
[^6]: Lin G (2023). _reactable: Interactive Data Tables for R_. R package version 0.4.4,
  <https://CRAN.R-project.org/package=reactable>.
[^7]: Luedeling E, Goehring L, Schiffers K, Whitney C, Fernandez E (2024). _decisionSupport: Quantitative Support of
  Decision Making under Uncertainty_. R package version 1.114,
  <https://CRAN.R-project.org/package=decisionSupport>.
[^8]: Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn
  M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D,
  Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686.
  doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
