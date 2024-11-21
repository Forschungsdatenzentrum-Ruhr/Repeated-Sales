## Repeated-Sales Algorithm for Germany

This repository shows the detailed generation of repeated sale indicator variables for the RWI-GEO-RED data.

# Abstract Algorithm Description

This paper introduces a repeated-sales algorithm tailored for the German real estate market, addressing the lack of such data. Utilizing the dataset RWI-GEO-RED from FDZ Ruhr at RWI, which exploits data from ImmobilienScout24.de, the algorithm classifies repeat-sales and repeat-rent listings, overcoming challenges like measurement errors and deviations in property characteristics. The methodology, based on nearest centroid classification, ensures accurate clustering of similar listings. The paper compares the new repeated-sales indices with the traditional hedonic price index, revealing more conservative estimates of rent growth. Additionally, the data’s panel structure allows for detailed analysis of unit-level dynamics and mispricing. The study highlights the advantages of this approach and suggests potential research avenues, providing a valuable tool for economists and policymakers to assess housing market trends in Germany.

# Access 

The variables introduced by the algorithm have been added to the RWI-GEO-RED data and will be included in all future waves starting with wave 12 (scheduled for release in spring 2025). The data are available to researchers for non-commercial use. There are two versions of the RWI-GEO-RED data. First, the Scientific Use Files (SUF) cover all information except the exact geo-coordinates. Second, the full datasets are available in the Data Secure Room of the FDZ Ruhr in Essen (on-site access). The data can be obtained as a Stata datasets (.dta), .csv files, and .parquet files. Data access to both versions requires a signed data use agreement. Both versions are restricted to non-commercial research and only researchers of scientific institutions are eligible to apply for data access. The SUF may be used at the workplace of the users. Data access is provided by the Research Data Centre Ruhr at the RWI – Leibniz-Institute for Economic Research (FDZ Ruhr). The data can be accessed at https://www.rwi-essen.de/en/research-advice/further/research-data-center-ruhr-fdz/data-access. The application form includes a brief description and title of the project, information on the applying department, expected duration of data usage as well as further participants in the project. Data users shall cite the datasets properly with the respective DOIs. The DOIs of wave 12 of the Scientific Use Files (SUF) are:
10.7807/immo:red:hk:suf:v12 for houses for sales
10.7807/immo:red:hm:suf:v12 for houses for rent
10.7807/immo:red:wk:suf:v12 for flats for sales
10.7807/immo:red:wm:suf:v12 for flats for rent
