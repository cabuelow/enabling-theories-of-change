## Code and data for ‘Global opportunities for conserving vegetated coastal wetlands’

Citation: Buelow et al. bioarchiv

An interactive web application and associated code can be found [here](https://github.com/cabuelow/enabling-profiles-app)

[About](#about) | [Scripts](#scripts) | [Data sources](#data-sources) | [License](LICENSE)

### About

This repository provides the code and data to reproduce all figures, tables, and supplemental figures/tables found in Buelow et al. 2021

[cbrown5](https://github.com/cbrown5) is a contributor to this code base

### Scripts

#### Run a Bayesian latent variable model (LVM) on raw indicator data to gap-fill missing indicator values by country


1. 01_lvm-model.R
    - The full model
2. 01_lvm-model-no-rams-manage.R
    - The model without the *Ramsar Management* indicator (to assess robustness to violation of the ‘missing at random’ assumption)
3. 01_lvm-model-no-cons-spend.R
    - The model without the *Conservation Spending* indicator (to assess robustness to violation of the ‘missing at random’ assumption)

#### Calculate the median predicted standard normal indicator value from the posterior distribution of the LVM, for each indicator and country

1. 02_predict-indicator-vals.R
    - Obtain the posterior distributions of latent variable scores and coefficients for each indicator and country (i.e. MCMC samples) to calculate the entire range of predicted standard normal indicator values and extract the median value. Repeat for each lvm model above. 
    - Also extract every 96th predicted value from the entire range of values for each indicator and country to assess Enabling Profile robustness (see 09_cluster-robustness.R)

#### Check that the raw and predicted standard normal indicator values are strongly positively correlated

1. 03_raw-vs-predicted.R

#### Evaluate robustness to violation of ‘missing at random’ assumption for *Ramsar Management* and *Conservation Spending* indicators

1. 04_evaluate-MAR-consspend.R
    - Assess robustness of model parameter estimation to violation of the ‘missing at random’ assumption for the *Conservation Spending* indicator (Fig S2)
2. 04_evaluate-MAR-ramsmanage.R
    - Assess robustness of model parameter estimation to violation of the ‘missing at random’ assumption for the *Ramsar Management* indicator (Fig S3)

#### Run a cluster analysis on the predicted standard normal indicator values to classify countries in to Enabling Profiles and produce a map

1. 05_cluster.R
    - Fig 1A

#### Determine the importance of indicators for classifying countries into Enabling Profiles with classification trees

1. 06_trees.R
    - Fig 1B and C

####  Identify and map the main drivers of mangrove and seagrass loss in each Enabling Profile

1. 07_drivers-loss.R
    - Fig 2 and 3

#### Produce an ordination of standard normal indicator values for each country, and group by Enabling Profile

1. 08_ordination.R
    - Fig S6

#### Assess the robustness of Enabling Profiles

1. 09_cluster-robustness.R
    - Fig S4 and 5

### Data sources

| Data category  | Indicator/Driver | Description | Source |
| ------------- | ------------- | ------------- | ------------- |
| Policy indicator | Nationally Determined Contribution (NDC) commitment | The commitment of a country to reducing green-house gas emissions, pledged as Nationally Determined Contributions (NDC) | Watson, R., McCarthy, J.J., Canziani P., Nakicenovic, N., Hisas, L. 2019. The truth behind the climate pledges. The Universal Ecological Fund (FEU-US) |
| Policy indicator | Nationally Determined Contribution (NDC) mitigation | Whether coastal wetlands are included in a country’s Nationally Determined Contribution (NDC) mitigation strategy | Herr, D., & Landis, E. 2016. Coastal blue carbon ecosystems Opportunities for Nationally Determined Contributions. Policy brief. Gland, Switzerland: IUCN and Washington, DC, USA: TNC. Retrieved from www.bluecsolutions.org |
| Policy indicator | Nationally Determined Contribution (NDC) adaptation | Whether coastal wetlands are included in a country's Nationally Determined Contribution (NDC) adaptation strategy | Herr, D., & Landis, E. 2016. Coastal blue carbon ecosystems Opportunities for Nationally Determined Contributions. Policy brief. Gland, Switzerland: IUCN and Washington, DC, USA: TNC. Retrieved from www.bluecsolutions.org |
| Policy indicator | Multilateral environmental agreements (MEAs) | Cumulative count of a country's multilateral environmental agreements (MEAs) since the year of its first MEA membership until 2019 | Ronald B. Mitchell. 2020. IEA Membership Count Dataset from the International Environmental Agreements Database Project (Version 20200214). Eugene: IEADB Project. Dataset generated on: 14 February 2020 |
| Policy indicator | Bilateral environmental agreements (BEAs) | Cumulative count of a country's bilateral environmental agreements (BEAs) since the year of its first BEA membership until 2019 | Ronald B. Mitchell. 2020. IEA Membership Count Dataset from the International Environmental Agreements Database Project (Version 20200214). Eugene: IEADB Project. Dataset generated on: 14 February 2020 |
| Policy indicator | Governance effectiveness | Composite indicator of the quality of a country's public services, civil service, degree of its independence from political pressures, quality of policy formulation and implementation, and credibility of its government’s commitment to such policies | Worldwide Governance Indicators, The World Bank. 2019. www.govindicators.org, accessed on [24/11/2020] |
| Policy indicator | Marine protected areas (MPA) target | The percent of a country’s Economic Exclusion Zone (EEZ) set aside as a marine protected area | Wendling, Z., Emerson, J., de Sherbinin, A., and Esty, D. 2020. Environmental Performance Index 2020. New Haven, CT: Yale Center for Environmental Law & Policy. Retrieved from epi.yale.edu |
| Policy indicator | Ramsar protection | The area of coastal wetlands under Ramsar jurisdiction in a country standardised by coastline length | The Ramsar Convention Secretariat. 2014. https://rsis.ramsar.org/, accessed on [04/04/2021] |
| Policy indicator | Ramsar management | Whether a country has Ramsar-listed coastal wetland sites (i.e., intertidal marshes, intertidal forested wetlands, and marine subtidal aquatic beds) with management plans implemented (0 = no, 1 = yes) | The Ramsar Convention Secretariat. 2014. https://rsis.ramsar.org/, accessed on [04/04/2021] |
| Regulation indicator | Ramsar management | Whether a country has Ramsar-listed coastal wetland sites (i.e., intertidal marshes, intertidal forested wetlands, and marine subtidal aquatic beds) with management plans implemented (0 = no, 1 = yes) | The Ramsar Convention Secretariat. 2014. https://rsis.ramsar.org/, accessed on [04/04/2021] |
| Regulation indicator | Regulatory quality | A composite indicator of regulatory quality that represent the ability of a country's government to formulate and implement sound policies and regulations that permit and promote private sector development | Worldwide Governance Indicators, The World Bank. 2019. www.govindicators.org, accessed on [24/11/2020] |
| Regulation indicator | Wastewater treatment | The percentage of wastewater that undergoes at least primary treatment in each country, normalised by the proportion of the population connected to a municipal wastewater collection system | Wendling, Z., Emerson, J., de Sherbinin, A., & Esty, D. 2020. Environmental Performance Index 2020. New Haven, CT: Yale Center for Environmental Law & Policy. Retrieved from epi.yale.edu |
| Regulation indicator | Wastewater treatment | The percentage of wastewater that undergoes at least primary treatment in each country, normalised by the proportion of the population connected to a municipal wastewater collection system | Wendling, Z., Emerson, J., de Sherbinin, A., & Esty, D. 2020. Environmental Performance Index 2020. New Haven, CT: Yale Center for Environmental Law & Policy. Retrieved from epi.yale.edu |
| Regulation indicator | Sustainable nitrogen use | How far a country is from achieving sustainable nitrogen use for crop fertilisation | Wendling, Z., Emerson, J., de Sherbinin, A., & Esty, D. 2020. Environmental Performance Index 2020. New Haven, CT: Yale Center for Environmental Law & Policy. Retrieved from epi.yale.edu |
| Regulation indicator | Sulphur dioxide emission reduction | The average annual rate of increase or decrease in SO2 | Wendling, Z., Emerson, J., de Sherbinin, A., & Esty, D. 2020. Environmental Performance Index 2020. New Haven, CT: Yale Center for Environmental Law & Policy. Retrieved from epi.yale.edu |
| Regulation indicator | Nitrous oxide emission reduction | The average annual rate of increase or decrease in N2O | Wendling, Z., Emerson, J., de Sherbinin, A., & Esty, D. 2020. Environmental Performance Index 2020. New Haven, CT: Yale Center for Environmental Law & Policy. Retrieved from epi.yale.edu |
| Regulation indicator | Environmental taxes | A measure of the number of taxes, charges, or fees for the following categories: 1) environmental management, 2) air pollution, 3) energy, 4) noise pollution, 5) fossil fuels, 6) transport management, 7) waste management, and 8) water management | OECD, Policy Instruments for the Environment (PINE) Database, http://oe.cd/pine , accessed on [24/11/2020] |
| Engagement indicator | Conservation spending | Average annual conservation investment (I$m) for countries that are signatories to the Convention on Biological Diversity (CBD) and Sustainable Development Goals (SDGs) | Waldron, A., Miller, D. C., Redding, D., Mooers, A., Kuhn, T. S., Nibbelink, N., Roberts, J. T., Tobias, J. A., & Gittleman, J. L. 2017. Reductions in global biodiversity loss predicted from conservation spending. Nature, 551, 364–367. https://doi.org/10.1038/nature24295 |
| Engagement indicator | Biodiversity projects | The number of biodiversity-related conservation projects in a developing country that are receiving funding from OECD (Organisation for Economic Cooperation and Development) countries, standardised by country area (km2) | http://www.oecd.org/dac/stats/biodiversity.htm, accessed on [24/11/2020] |
| Engagement indicator | Environmental NGO projects | The number of environmental NGO aid projects in a country standardised by country area (km2). Note that data is provided on a voluntary basis and so is not fully comprehensive | https://ngoaidmap.org/, accessed on [24/11/2020] |
| Engagement indicator| Biodiversity engagement | A score based on the number of times biodiversity key words have been sourced from twitter, online newspapers and google in a country (per month) | http://biodiversityengagementindicator.com/, accessed on [24/11/2020] |
| Drivers of wetland loss | Mangrove loss drivers | N/A | Goldberg, L., Lagomasino, D., Thomas, N., and Fatoyinbo, T. (2020). Global declines in human-driven mangrove loss. Glob. Chang. Biol. 26, 5844–5855 |
| Drivers of wetland loss | Seagrass drivers | N/A | Dunic, J. C., Brown, C. J., Connolly, R. M., Turschwell, M. P., and Côté, I. M. (2021). Long-term declines and recovery of meadow area across the world’s seagrass bioregions. Global Change Biology 27, 4096–4109. doi:10.1111/gcb.15684 |
