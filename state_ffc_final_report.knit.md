---
title: "State-level Methodology Report"
author: "EPA| OAR | OAP | Climate Change Division | Climate Policy Branch"
date: "2024-07-18"
output: 
  pdf_document:
    keep_tex: true
header-includes:

  - \usepackage{xcolor} % For font color
  - \usepackage[dvipsnames]{xcolor}
  - \usepackage{sectsty} % For section font size
  - \usepackage{titlesec} % For customizing title spacing
  - \sectionfont{\color{TealBlue}\LARGE}
  - \subsectionfont{\color{CornflowerBlue}\Large}
  - \subsubsectionfont{\color{Aquamarine}\large}
  
---



# 2. Energy (NIR Chapter 3) \newline

For this methodology report, energy emissions are broken into two main categories: emissions associated with fuel use---including fossil fuel combustion (FFC) and non-energy use (NEU)---and fugitive emissions mainly from fuel production. The energy emissions presented here include some categories that are not added to energy sector totals in the national Inventory but are instead presented as memo items, including international bunker fuels (IBFs) and biomass emissions, consistent with UNFCCC reporting guidelines. This approach directly affects state-level energy sector estimates and, in some cases, may account for differences with official estimates published by individual state governments. For more information on energy sector emissions, see Chapter 3 of the national Inventory. Table 2-1 summarizes the different approaches used to estimate state-level energy emissions and completeness across states. Geographic completeness is consistent with the national Inventory. The sections below provide more detail on each category.



\begin{table}
\centering
\caption{\label{tab:background}Overview of Approaches for Estimating State-Level Energy Sector GHG Emissions}
\centering
\begin{tabular}[t]{>{\raggedright\arraybackslash}p{3cm}>{\raggedright\arraybackslash}p{3cm}>{\raggedright\arraybackslash}p{4cm}>{\raggedright\arraybackslash}p{4cm}}
\toprule
Category & Gas & Approach & Geographic Completeness\\
\midrule
\cellcolor{gray!10}{FFC} & \cellcolor{gray!10}{NA} & \cellcolor{gray!10}{Hybrid approach: Approach 1 used for most fuels and sectors,Approach 2 proxy data used to allocate national totals for some fuels and sectors} & \cellcolor{gray!10}{Includes emissions from all states, the District of Columbia, tribal lands, and territories (i.e., American Samoa, Guam, Puerto Rico, , Northern Mariana Islands, U.S. Virgin Islands and other outlying minor islands) as applicable.}\\
NEUs of Fossil Fuels & CO2 & Approach 2 & Includes emissions from all states, the District of Columbia, tribal lands, and territories (i.e., American Samoa, Guam, Puerto Rico, , Northern Mariana Islands, U.S. Virgin Islands and other outlying minor islands) as applicable.\\
\cellcolor{gray!10}{Geothermal Emissions} & \cellcolor{gray!10}{CO2} & \cellcolor{gray!10}{Approach 2} & \cellcolor{gray!10}{Includes emissions from all states, the District of Columbia, and tribal lands as applicable.}\\
Incineration of Waste & CO2, CH4, N2O & Hybrid approach: 2011–2021: Approach 1, 1990–2010: Approach 2 & Includes emissions from all states, the District of Columbia, and tribal lands as applicable.\\
\cellcolor{gray!10}{IBFs (memo item)} & \cellcolor{gray!10}{CO2, CH4, N2O} & \cellcolor{gray!10}{Approach 2} & \cellcolor{gray!10}{Includes emissions from all states, the District of Columbia, and tribal lands as applicable.}\\
\addlinespace
Wood Biomass and Biofuels Consumption (memo item) & CO2 & Approach 2 & Includes emissions from all states, the District of Columbia, and tribal lands as applicable.\\
\bottomrule
\end{tabular}
\end{table}

## 2.1 Emissions Related to Fuel Use \newline

This section presents the methodology used to estimate the fuel use portion of emissions, which consists of the following sources:

-   FFC (CO~2~, CH~4~, N~2~O)

-   Carbon emitted from NEUs of fossil fuels (CO~2~)

-   Geothermal emissions (CO~2~)

-   Incineration of waste (CO~2~, CH~4~, N~2~O)

-   IBFs (CO~2~, CH~4~, N~2~O)

-   Wood biomass and biofuels consumption (CO~2~)

### 2.1.1 Fossil Fuel Combustion (NIR Section 3.1) \newline

#### 2.1.1.1 Background \newline

Emissions from FFC include the GHGs CO~2~, CH~4~, and N~2~O. CO~2~ is the primary gas emitted from FFC and represents the largest share of U.S. total GHG emissions. The methods to estimate CO~2~ emissions from FFC and the methods to estimate CH~4~ and N~2~O emissions from stationary and mobile combustion rely in large part on the same underlying data. However, there are some differences; therefore, the methods used to estimate CO~2~ and non-CO~2~ emissions are presented separately.

#### 2.1.1.2 Methods/Approach \newline

The approach for determining national-level FFC emissions is based on multiplying emissions factors times activity data on fuel consumption. The activity data on fuel consumption were taken from national-level energy balances prepared for EIA's Monthly Energy Review (MER) estimates (EIA 2023a). EIA prepares national-level energy statistics that consider energy production imports/exports and stock changes to determine energy supply/consumption. The fuel consumption information is used as a starting point for determining emissions. The approach starts with determining fuel use by fuel type because different types of fuels have different carbon content (C content) and therefore different emissions factors. The information is also broken out by energy‐consuming sectors of U.S. society to provide more detail and information on trends; the sectors included are residential, commercial, industrial, transportation, and electric power. Data from U.S. territories were also included in the analysis per international reporting requirements. Several adjustments were made to the data to account for fuel use and emissions that are either excluded or reported in other parts of the national Inventory, as shown in Figure 2-1.

![Figure 2-1. Adjustments to Energy Consumption for Emissions Estimates](images/paste-4D67E161.png)

This section describes how national-level estimates for FFC were disaggregated to the state level for the following separate sources:

• FFC CO2

• Stationary non-CO2 emissions

• Mobile non-CO2 emissions

This section also discusses how energy use data were broken out at the state level as part of the adjustments noted in Figure 2-1 and then used to report emissions elsewhere in the national Inventory. Emissions from energy use that were excluded from FFC are discussed in other sections of the report as follows:

• For energy used in the IPPU sector, see Chapter 3.

• For biofuel use, see Section 2.1.6.

• For NEUs of fuels, see Section 2.1.2.

For IBFs, see Section 2.1.5.

Disaggregating FFC emissions to the state level largely followed the same process and energy consumption data that are used at the national level. However, in several instances, the data used to develop national estimates are not available at the state level, and additional steps were needed to distribute national-level emissions across the states while maintaining consistency with national-level totals. Therefore, Approach 3, the Hybrid approach as described in Section 1.3 of the Introduction chapter, was used to determine state-level emissions for FFC, including some data that were directly used in the national Inventory and some surrogate data as discussed in the following sections.

##### 2.1.1.2.1 FFC CO~2~ State-Level Breakout \newline

CO~2~ emissions from FFC at the national level are estimated with a Tier 2 method described by the IPCC in the 2006 IPCC Guidelines for National Greenhouse Gas Inventories (IPCC 2006). As discussed above, this method is based on multiplying activity data on fuel use (that have been adjusted to allocate and report data consistent with UNFCCC reporting guidelines and avoid double counting) by emissions factors to determine emissions. Determining adjusted fuel use activity data is based on the seven steps discussed in Table 2-2 below. The result of these seven steps is an adjusted amount of fuel use activity data that are then used to determine FFC CO~2~ emissions. In Appendix A to this document (included as separate Excel files), the "National 2021 FFC CO~2~" Tab provides more details on an example of the adjustments made to the national-level energy use data to determine adjusted fuel use activity data for 2021.

Determining adjusted fuel use activity data is based on the seven steps discussed in Table 2-2 below. The result of these seven steps is an adjusted amount of fuel use activity data that are then used to determine FFC CO2 emissions. In Appendix A to this document (included as separate Excel files), the "National 2021 FFC CO2" Tab provides more details on an example of the adjustments made to the national-level energy use data to determine adjusted fuel use activity data for 2021. Three additional steps (Steps 8--10 in Table 2-2) are required to determine CO2 emissions in the national Inventory, also discussed below.

Ideally, to determine state-level FFC CO2 emissions estimates, the same approach could be used, and adjusted energy use, as shown in the "National 2021 FFC CO2" Tab of Appendix A, could be developed for each state. However, the national-level emissions were developed based on multiple factors and inputs, some of which were not available or readily published at the state level. Therefore, a Hybrid approach was taken where state-level data were used when available. In cases where state-level data were not available, national-level estimates were used with available surrogate data to determine state-level percentages of each fuel use. Table 2-2 shows a high-level comparison of the different data sources used for the different steps to determine national-level and state-level estimates.

\begin{table}
\centering
\caption{\label{tab:breakout}Comparison of Approaches/Data Sources Used to Determine FFC Emissions}
\centering
\begin{tabular}[t]{>{\raggedright\arraybackslash}p{3cm}>{\raggedright\arraybackslash}p{3cm}>{\raggedright\arraybackslash}p{4cm}>{}p{4cm}}
\toprule
Calculation Step & National-Level Estimates & State-Level Estimates\\
\midrule
\cellcolor{gray!10}{Step 1: Determine Total Fuel Consumption by Fuel Type and Sector} & \cellcolor{gray!10}{Based on EIA MER} & \cellcolor{gray!10}{Based on EIA SEDS (adjusted to match national totals as applicable)}\\
Step 2: Subtract Uses that are Accounted for in the IPPU Sector & Taken from industry data or based on national-level emissions & National-level data allocated to states based on state-level emissions estimates for each IPPU category in question as calculated in Chapter 3\\
\cellcolor{gray!10}{Step 3: Adjust for Biofuels and Petroleum Denaturant} & \cellcolor{gray!10}{Based on national-level data from EIA MER} & \cellcolor{gray!10}{Not needed (see Step 5)}\\
Step 4: Adjust for CO2 Exports & Based on industry data and Canadian import data & Based on industry data and Canadian import data\\
\cellcolor{gray!10}{Step 5: Adjust Sectoral Allocation of Diesel Fuel and Gasoline} & \cellcolor{gray!10}{Based on bottom-up transportation sector data on fuel use by vehicle type} & \cellcolor{gray!10}{National-level data (already excluding biofuels) allocated to states based on state-level fuel use data (not vehicle specific)}\\
\addlinespace
Step 6: Subtract Consumption for NEUs & Based on data from EIA MER & National-level data allocated to states based on SEDS\\
\bottomrule
\end{tabular}
\end{table}

The following discussion details what data were used for each step in Table 2-2 to determine national- and state-level FFC emissions. Appendix A, Table A-1 in the "State FCC CO2" Tab, provides more details on where state-level data were used directly and where other data were used to make adjustments to disaggregate national numbers across fuel types and sectors for each of the steps identified.

##### 2.1.1.2.2 Step 1: Determine Total Fuel Consumption by Fuel Type and Sector \newline

As discussed above, national-level data on fuel supply/consumption comes from EIA's MER. Because not all fuel supplied/consumed directly results in GHG emissions, or it could be included as part of other emissions reporting in the national Inventory, adjustments have to be made as shown above in Table 2-2 and described in the following steps. State-level energy data are available from EIA's State Energy Data System (SEDS). Those data are broken out by fuel type and sector (residential, commercial, industrial, transportation, and electric power) and are available for the years 1960--2021 (EIA 2023b). SEDS estimates energy consumption using data from surveys of energy suppliers that report consumption, sales, or distribution of energy at the state level. Most SEDS estimates rely directly on collected state-level consumption data. For example, SEDS uses state-level sales survey data and other proxies of consumption to allocate the national petroleum product supplied totals to the states. The sums of the state estimates equal the national totals as closely as possible for each energy type and end-use sector, and energy consumption estimates are generally comparable to the national statistics in EIA's MER because both data sets rely largely on the same survey returns for producers and consumers.

However, the totals across all states (and the District of Columbia) from SEDS do not always match the U.S. total energy data used in the national Inventory, which is based on the EIA February 2023 MER estimates (EIA 2023b). The main differences are for coal and natural gas and primarily in the industrial sector, as shown in Figure 2-2 below. For coal, there are differences in both energy content and short tons, but the differences are not consistent across time or sectors. For natural gas, the difference is mainly in the energy content. The reason for the differences is that SEDS uses state-level energy content conversion factors for coal and natural gas, while the MER uses national-level conversion factors. These different calculations sometimes cause the sums of the SEDS states to be different than the MER values. Although the percentage differences are not large (max 5.2% for coal and 1.4% for natural gas in the industrial sector), they cause noticeable differences when comparing emissions totals across all states to national totals, especially by sector.

\begin{figure}
\includegraphics[width=0.8\linewidth]{state_ffc_final_report_files/figure-latex/plots2-1} \caption{Figure 2-2. Differences Between State-Level and National Total Energy Use for Coal and Natural Gas.}\label{fig:plots2}
\end{figure}

The petroleum categories generally line up well across state-level and national totals. There are only minor differences in petroleum coke, mainly in the industrial sector, as shown in Figure 2-3 below. For petroleum coke, there are differences in energy content and barrels, but the difference in energy content appears in 2004, which is when petroleum coke heating values were changed from a constant value to values based on marketable and catalyst coke. Again, this difference is because of different national-level and state-level conversion factors. Since 2004, the MER has used an annual national-level "quantity-weighted" average petroleum coke conversion factor (instead of a fixed factor). SEDS applies the marketable and catalyst coke conversion factors to the state-level consumption of each petroleum coke category within each state.

</div>

\begin{figure}
\includegraphics[width=0.8\linewidth]{state_ffc_final_report_files/figure-latex/plots3-1} \caption{Figure 2-3. Differences Between State-Level and National Total Energy Use for Petroleum Coke.}\label{fig:plots3}
\end{figure}

For diesel fuel and gasoline, the totals generally line up, but there are differences across sectors. These differences are discussed in Step 5 below. \newline

In addition to the differences in gasoline and diesel fuel across sectors over the time series, there are also differences in some petroleum fuels across sectors, specifically in 2021. This is because the SEDS represents the latest data from EIA in terms of sector breakouts that were not reflected in the national Inventory 2021 values that relied on older EIA data. Again, the totals for the fuels line up, but there are differences across sectors, as shown in Figure 2-4 below. The updated SEDS data were used in the state-level breakout because they represent the latest data available. This results in differences in 2021 results across sectors for the state totals versus the national Inventory. However, the national Inventory numbers will be updated to match the 2021 SEDS data during the next national Inventory cycle.

\begin{figure}
\includegraphics[width=0.8\linewidth]{state_ffc_final_report_files/figure-latex/plots4-1} \caption{Figure 2-4. 2021 Differences Between Sectors for Petroleum Fuels (SEDS—National Inventory).}\label{fig:plots4}
\end{figure}

Furthermore, some of the fuel use reported in SEDS is different from the reporting in the national Inventory. For example, natural gas reported in SEDS includes supplemental gas, which is included in the national Inventory under the primary fuel used to make the supplemental gas, so including supplemental gas in state level results would result in double counting. Liquefied petroleum gas (LPG) in SEDS is reported differently over time, including as total hydrocarbon gas liquids (HGLs) that include natural gasoline and as a mix of different gases. Natural gasoline (called pentanes plus in the national Inventory) is accounted for separately from other HGLs in the national Inventory. Gasoline and distillate fuels in SEDS include biofuels (fuel ethanol, biodiesel and renewable diesel, and other biofuels are included in the MER but not estimated in SEDS yet), which were reported separately in the national Inventory. These differences make it difficult to use the SEDS data directly to determine state-level fuel use data, in a manner consistent with the national Inventory.

Therefore, the following approach was used in determining fuel use by type by sector at the state level:

• If SEDS data totals matched the national totals and there were no further adjustments needed (as per Steps 2--7), the SEDS data were used directly to represent state-level energy use.

• For fuels where the SEDS totals did not match the national totals (i.e., coal, natural gas, and petroleum coke), fuel use in each sector was adjusted to match the national totals used in the national Inventory. This calculation was based on the percentage of each fuel used in each state from the SEDS data. For the industrial sector, this adjustment was made after subtracting for uses in the IPPU sector (see Step 2 below).

• For other fuels where sector totals did not match up (e.g., gasoline and diesel fuel), totals for each fuel type were generally taken from the national Inventory (see Step 5), and the SEDS data or other proxy data sources were used to determine state-level percentages of each fuel use.

This approach generally results in state-level energy use data that are consistent with national totals used in the national Inventory. More details on further adjustments made during the different steps are discussed below.

Appendix A has details on how the SEDS data were adjusted to determine state-level energy use by fuel type and sector. Tables A-2 through A-6 in the "FFC CO2 Residential" Tab describe the residential sector adjustments. Tables A-9 through A-13 in the "FFC CO2 Commercial" Tab describe the commercial sector adjustments. Tables A-44 through A-47 in the "FFC CO2 Industrial" Tab describe the industrial sector adjustments for petroleum coke and HGL; the remaining industrial sector adjustments are described further in Steps 2 and 3 below. Tables A-50 and A-51 in the "FFC CO2 Transportation" Tab describe the transportation sector adjustments. Tables A-52 through A-56 in the "FFC CO2 Electricity" Tab describe the electricity production sector adjustments.

##### 2.1.1.2.3. Step 2: Subtract Uses That Are Accounted for in the IPPU Sector \newline

In the national Inventory, portions of fuel consumption data for several fuel categories (coking coal, other coal, natural gas, residual fuel, and distillate fuel) are reallocated from the energy sector to the sector because these portions were consumed as raw materials during nonenergy-related industrial processes. As per IPCC Guidelines that distinguish between the energy and IPPU sector reporting, emissions from fuels used as raw materials are presented as part of IPPU and are removed from the energy use estimates (IPCC 2006, Volume 3, Chapter 1). Portions of fuel use were therefore subtracted from the industrial sector fuel consumption data before determining combustion emissions. Note that other adjustments were also made to the NEU calculations to reflect energy use accounted for under IPPU; see Step 6 and the NEU emissions discussion below.

The adjustments vary over time and represent from about 4% to 8% of total unadjusted industrial sector energy use, as shown in Figure 2-5.

\begin{figure}
\includegraphics[width=0.8\linewidth]{state_ffc_final_report_files/figure-latex/plots5-1} \caption{Figure 2-4. 2021 Differences Between Sectors for Petroleum Fuels (SEDS—National Inventory).}\label{fig:plots5}
\end{figure}

Adjustments for each fuel type were made based on industry data or assumptions about fuel use based on emissions reported under IPPU. The following bullets discuss the assumptions made regarding the different industrial sector fuel types at the national and state levels to reflect their use in IPPU:

• Coking coal. Coking coal is used to make coke that, in turn, is used in industrial processes. The national total amount of coking coal used in IPPU was back-calculated based on the amount of coking coal needed to make the coke used as input to iron and steel (I&S) and lead and zinc production (approximately 94% is used in I&S). National-level coke use in I&S production was based on industry data that are not available at the state level. Coke used in lead and zinc production was based on the amount of carbon emitted from the processes and Is also not available specifically at the state level. Therefore, the national total amount of coking coal used in IPPU was allocated per state based on the percentage of total coking coal used per state from the SEDS data. This approach assumes that coke use in I&S and lead and zinc production is proportional to the amount of coking coal used in a state. This assumption may not be the case because state-level coking coal use is based on coke production in a given state, not necessarily coke use. The coke could be produced in one state and shipped for use in another state. However, given the lack of specific data, coking coal use was determined to be a good surrogate for coke use within a given state because coke production is often integrated with I&S production where the coke is used. As one further adjustment, if the amount of coking coal used in IPPU was greater than the total coking coal reported in the national energy statistics, the amount of coking coal used in the energy sector results were zeroed out to avoid negative values (this only occurs in 1990, 1991, 1992, and 1997), and additional other coal use was subtracted to make up the difference (see "Other coal" below). Appendix A, Tables A-19 and A-20 in the "FFC CO2 Industrial" Tab, describe the coking coal used in IPPU.

• Other coal. Two adjustments were made to account for other coal used in the industrial sector. The first adjustment was to subtract the extra amount of coking coal required for years where the coking coal adjustment was more than the coking coal total (see above). Similar to coking coal, this adjustment was based on the percentage of coking coal consumption per state from SEDS. Appendix A, Tables A-21 and A-22 in the "FFC CO2 Industrial" Tab, describe this adjustment. The second adjustment was to subtract coal directly used in the I&S sector. In addition to being used indirectly to produce coke, coal can be used directly as a process input to I&S production; note that this does not include coal combusted at I&S facilities to produce power. Other national-level coal used in I&S production was based on industry data that are not available at the state level. Therefore, this adjustment was based on the percentage of I&S emissions per state. I&S emissions per state were taken from the IPPU breakout for I&S, as described in Section 3.3.1, and the percentage for basic oxygen furnaces (BOFs) was assumed to best represent other coal use in I&S. BOF emissions were determined to be a good surrogate for other coal direct use in I&S because coal is primarily used in the BOF process and would be proportional to emissions from the process. Appendix A, Table A-24 in the "FFC CO2 Industrial" Tab, describes this adjustment. An IPPU-adjusted other coal total was then calculated by subtracting the adjustments described above (note: this also included the adjustments for conversion of fuels and CO2 exports as described in Step 4 below). Appendix A, Table A-25 in the "FFC CO2 Industrial" Tab, shows this total. The total other coal use was then adjusted to match the total other coal from the national Inventory (as per Step 1); this adjustment was based on the percentage of other coal used after the IPPU adjustment. Appendix A, Table A-26 in the "FFC CO2 Industrial" Tab, describes this adjustment.

• Natural gas. Two adjustments were made to account for natural gas used in the industrial sector. The first adjustment was to subtract the amount of natural gas consumption that was used in ammonia production from energy sector natural gas use. The national-level natural gas used in ammonia production was back- calculated based on assumed CO2 emissions from ammonia production and calculations on the amount of C content in natural gas needed to produce those CO2 emissions. Therefore, the state-level natural gas used for ammonia was based on the percentage of ammonia emissions per state. Ammonia emissions per state were taken from the IPPU breakout for ammonia, as described in Section 3.2.1. Appendix A, Tables A-27 through A-29 in the "FFC CO2 Industrial" Tab, describe this adjustment. The second adjustment was to subtract natural gas directly used in I&S. National-level natural gas used in I&S production was based on industry data that are not available at the state level. Therefore, similar to other coal, the adjustment was based on the percentage of I&S emissions per state from the IPPU breakout for I&S, as described in Section 3.3.1, and the percentage for BOFs was assumed to best represent natural gas use in I&S. Similar to other coal direct use, BOF emissions were determined to be a good surrogate for natural gas direct use in I&S. Appendix A, Table A-30 in the "FFC CO2 Industrial" Tab, describes this adjustment. An IPPU-adjusted natural gas total was then calculated by subtracting the adjustments described above. Appendix A, Table A-31 in the "FFC CO2 Industrial" Tab, shows this total. The total natural gas use was then adjusted to match the total natural gas use from the national Inventory (as per Step 1); this adjustment was based on the percentage of natural gas used after the IPPU adjustment. Appendix A, Table A-32 in the "FFC CO2 Industrial" Tab, describes this adjustment.

• Residual fuel. The residual fuel use was adjusted to subtract the amount of residual fuel used in carbon black production. Carbon black was the only IPPU use of residual oil. The national-level residual oil used in IPPU was based on NEUs of residual oil from EIA data, which are not available at the state level. Therefore, the residual oil IPPU state-level adjustment was based on the percentage of carbon black emissions per state. Carbon black emissions per state were taken from the IPPU breakout for petrochemicals, as described in Section 3.2.9, and the percentage for carbon black specifically was used. Carbon black emissions were determined to be a good surrogate for residual oil use because the emissions from carbon black production would be directly proportional to residual oil use. Appendix A, Tables A-33 and A-34 in the "FFC CO2 Industrial" Tab, describe this adjustment. An IPPU-adjusted residual fuel total was then calculated. Appendix A, Table A-35 in the "FFC CO2 Industrial" Tab, shows this total. The total residual fuel use was then adjusted to match the total residual fuel from the national Inventory (similar to what was done for coal and natural gas in Step 1); this adjustment was based on the percentage of residual fuel used after the IPPU adjustment. After the adjustment, the residual fuel use summed across states did not match the national totals anymore (likely due to the distribution of adjustment based on petrochemical production, which resulted in negative emissions in some states that were then zeroed out). Appendix A, Table A-36 in the "FFC CO2 Industrial" Tab, describes this adjustment.

• Distillate fuel. Distillate fuel use was adjusted to subtract the amount of distillate fuel directly used in I&S production. National-level diesel fuel used in I&S production was based on industry data that are not available at the state level. Therefore, similar to other coal and natural gas direct use in I&S, the adjustment was based on the percentage of I&S emissions per state from the IPPU breakout for I&S, as described in Section 3.3.1, and the percentage for BOFs was assumed to best represent distillate fuel use. Similar to other coal and natural gas direct use in I&S, BOF emissions were determined to be a good surrogate for diesel fuel direct use in I&S. Appendix A, Tables A-37 and A-38 in the "FFC CO2 Industrial" Tab, describe this adjustment. An IPPU-adjusted distillate fuel total was then calculated. Appendix A, Table A-39 in the "FFC CO2 Industrial" Tab, shows this total. This total was adjusted further based on reallocation of diesel fuel use across sectors, as shown in Step 5 below.

##### 2.1.1.2.4. Step 3: Adjust for Biofuels and Petroleum Denaturant \newline

Fuel consumption estimates used for CO2 calculations were adjusted downward to exclude fuels with biogenic origins consistent with the IPCC Guidelines. CO2 emissions from ethanol and biodiesel consumption are not included in fuel combustion totals in line with the 2006 IPCC Guidelines and UNFCCC reporting obligations to avoid double counting with net carbon fluxes from changes in biogenic carbon reservoirs accounted for in the estimates for LULUCF. CO2 emissions from biogenic fuels under fuel combustion are estimated separately and reported as memo items for informational purposes under the energy sector. Furthermore, for several years of the time series, denaturant used in ethanol production was double counted in both transportation and industrial sector energy use statistics. It was therefore subtracted from transportation sector energy use to avoid double counting. Fuels with biogenic origins (ethanol and biodiesel) and ethanol denaturant adjustments at the state level are handled by adjusting gasoline and diesel fuel use based on the total non-biogenic components of those fuels only (which also include any adjustments for denaturant), as described in Step 5 below. So, in effect, the state-level energy use calculations used to determine FFC emissions for gasoline and diesel fuel combine this Step 3 with Step 5 below. See Section 2.1.6 for more detail on biofuel use at the state level used to calculate biomass CO2 as a memo item.

##### 2.1.1.2.5. Step 4: Adjust for Biofuels and Petroleum Denaturant \newline

Since October 2000, the Dakota Gasification Plant has been exporting CO2 produced in a coal gasification process to Canada by pipeline. Because this CO2 is not emitted to the atmosphere in the United States, the coal that is gasified to create the exported CO2 is subtracted from fuel consumption statistics used to calculate combustion emissions in the national Inventory. Consistent with the approach currently used in the national Inventory, the coal used to produce exported CO2 from the Dakota gas plant to Canada was subtracted from other coal use to determine state-level emissions. This was all assumed to be subtracted from North Dakota, the location of the Dakota gas plant. Appendix A, Table A-23 in the "FFC CO2 Industrial" Tab, describes this adjustment.

##### 2.1.1.2.6. Step 5: Adjust Sectoral Allocation of Distillate Fuel Oil and Motor Gasoline \newline

In the national Inventory, portions of fuel consumption data for several fuel categories (coking coal, other coal, natural gas, residual fuel, and distillate fuel) are reallocated from the energy sector to the sector because these portions were consumed as raw materials during nonenergy-related industrial processes. As per IPCC Guidelines that distinguish between the energy and IPPU sector reporting, emissions from fuels used as raw materials are presented as part of IPPU and are removed from the energy use estimates (IPCC 2006, Volume 3, Chapter 1). Portions of fuel use were therefore subtracted from the industrial sector fuel consumption data before determining combustion emissions. Note that other adjustments were also made to the NEU calculations to reflect energy use accounted for under IPPU; see Step 6 and the NEU emissions discussion below.

Motor gasoline and diesel fuel are used across all sectors. The total amount of motor gasoline and diesel fuel consumed as reported in the MER is based on petroleum supply data from refineries. Gasoline use is allocated across the sectors in proportion to aggregations of categories reported in the U.S. Department of Transportation's Federal Highway Administration (FHWA) highway statistics data (FHWA 1996--2021).14 Diesel fuel use is allocated to the electric power sector based on industry surveys. The remaining diesel fuel use is allocated across the remaining sectors in a similar way to gasoline use based on sales data to different categories. Through 2020, the allocation was based on data from EIA's fuel oil and kerosene sales (FOKS) data (EIA 2022). EIA suspended the FOKS report after data year 2020. Starting in 2021, diesel fuel use is allocated to sectors based on data from SEDS. For 2021 forward, SEDS uses several external sources, regressions, and historical sector and state shares to estimate the data that were in the FOKS report. For the national Inventory, data are needed on fuel use by vehicle type to determine emissions, so a bottom-up method is used to estimate transportation sector gasoline and diesel fuel use. The national Inventory determines gasoline and diesel fuel use by vehicle type based on FHWA data and outputs from EPA's MOtor Vehicle Emissions Simulator (MOVES) model (EPA 2022). The national Inventory then allocates the remaining fuel use to the remaining sectors based on the proportions in the EIA data. The differences in the EIA and national Inventory gasoline and diesel fuel allocation approach across sectors are shown below in Figure 2-6 and Figure 2-7, including information on the categories of use included in each sector and data for 2021 as an example.

INFOGRAPHIC 2-6

INFOGRAPHIC 2-7

The bottom-up approach used by the national Inventory to determine transportation sector fuel use generally results in less allocation of gasoline to the transportation sector (and more to other sectors) and more diesel fuel allocated to the transportation sector (and less to other sectors) compared with the original MER energy balance data, as shown below in Figure 2-8.

\begin{figure}
\includegraphics[width=1\linewidth]{state_ffc_final_report_files/figure-latex/plots8-1} \caption{Figure 2-8. Comparison of Transportation Sector Fuel Use.}\label{fig:plots8}
\end{figure}

The national-level data on gasoline and diesel fuel use by vehicle type used in the bottom-up analysis was not readily available at the state level. Therefore, the following assumptions and adjustments were made to distillate fuel and motor gasoline consumption at the state level across the different sectors to reflect the national Inventory bottom-up transportation fuel use approach:

• Transportation sector. The total amount of distillate fuel and motor gasoline used in the transportation sector was taken from the national Inventory totals (these totals already subtract biofuel use, subtract denaturants if needed, and are based on multiple factors to determine transportation sector fuel use). This total amount of distillate fuel and motor gasoline use and emissions was allocated across states based on the percentage of fuel use by state in gallons from FHWA data (FHWA 2021a, 2021b). For distillate fuel, the total was based on FHWA form MF-225, and the motor gasoline total was based on FHWA form MF-226, both of which have time series of fuel use by state. Appendix A, Tables A-48 and A-49 in the "FFC CO2 Transportation" Tab, describe this adjustment. The FHWA data reflect on-highway fuel use, but, as seen in Figure 2-6 and Figure 2-7 above, the transportation sector fuel use includes some mobile sources that are considered off-highway (e.g., recreational boating, railroads). However, because the majority of the motor gasoline and diesel fuel use is for on-highway purposes, using FHWA data to allocate transportation sector fuel use to the state level is reasonable. Note that FHWA state-level fuel consumption data are representative of the point-of-sale and not the point-of-use, so fuel sold in one state that may be combusted in other states is assigned to the state where the fuel was purchased. This approach is consistent with IPCC Guidelines (IPCC 2006) for country-level reporting that indicate that "where cross-border transfers take place in vehicle tanks, emissions from road vehicles should be attributed to the country where the fuel is loaded into the vehicle." Therefore, when applying the IPCC approach to the state-level inventory, vehicle emissions are attributed to the state where the vehicle fuel is sold. This approach could introduce some differences in state-level transportation sector fuel use and emissions allocations reported here and those reported by individual states. For example, in addition to fuel sales data, state-level vehicle miles traveled (VMT) data are another potential surrogate for allocating fuel use to the state level, but that approach does not account for vehicle and fleet fuel economy variability between states. EPA will consider alternative or complementary approaches to allocate transportation fuel across states, including VMT data and other sources. For example, the National Emissions Inventory (NEI) uses county-level fleet and activity data to generate a bottom-up inventory (EPA 2017).Figure 2-9 shows the transportation sector emissions in 2020 from the top 10 emitting states using different allocation approaches. As seen in the figure, the approach used will lead to different allocations across states.



Residential sector. The total amount of distillate fuel used in the residential sector was taken from the national Inventory totals. It was allocated across states based on the percentage of existing fuel use in the residential sector per state from SEDS. Appendix A, Tables A-7 and A-8 in the "FFC CO2 Residential" Tab, describe this adjustment. Based on the reallocation of sector fuel use, the residential sector fuel use from the national Inventory is different from the value in SEDS; therefore, the state-level allocation from SEDS may not represent exactly the fuel values from the national Inventory. However, residential sector fuel use represented by the national Inventory should be consistent with what is included in SEDS (e.g., home heating); therefore, the SEDS state-level breakout is assumed to be representative.

• Commercial sector. The total amount of distillate fuel and motor gasoline used in the commercial sector was taken from the national Inventory totals. It was allocated across states based on the percentage of existing fuel use in the commercial sector per state from SEDS. Appendix A, Tables A-14 to A-18 in the "FFC CO2 Commercial" Tab, describe this adjustment. Based on the reallocation of sector fuel use, the commercial sector fuel use from the national Inventory is different from the value in SEDS; therefore, the state-level allocation from SEDS may not represent the exact fuel values from the national Inventory. However, commercial sector fuel use represented by the national Inventory should be consistent with what is included in SEDS (e.g., construction equipment); therefore, the SEDS state-level breakout is assumed to be representative.

• Industrial sector. The total amount of distillate fuel and motor gasoline used in the industrial sector was taken from the national Inventory totals. Distillate fuel was allocated across states based on the percentage of existing fuel use in the industrial sector per state after the IPPU adjustments described in Step 2. Motor gasoline was allocated across states based on the percentage of existing fuel use in the industrial sector per state from SEDS. Appendix A, Tables A-40 and A-43 in the "FFC CO2 Industrial" Tab, describe this adjustment. Based on the reallocation of sector fuel use, the industrial sector fuel use from the national Inventory is different from the value in SEDS; therefore, the state-level allocation from SEDS may not represent the exact fuel values from the national Inventory. However, industrial sector fuel use represented by the national Inventory should be consistent with what is included in SEDS (e.g., process energy use); therefore, the SEDS state-level breakout is assumed to be representative.

• Electric power sector. The total amount of distillate fuel used in the electric power sector was taken from the national Inventory totals. It was allocated across states based on the percentage of existing fuel use in the electric power sector per state from SEDS. Appendix A, Tables A-57 and A-58 in the "FFC CO2 Electricity" Tab, describe this adjustment. The electric power sector fuel use was not adjusted in the national Inventory compared with what is represented in SEDS; therefore, the SEDS state-level breakout is considered representative.

##### 2.1.1.2.7. Step 6: Subtract Consumption for NEU \newline

The energy statistics include consumption of fossil fuels for nonenergy purposes. Most fossil fuels consumed are combusted to produce heat and power. However, some are used directly for NEU as construction materials, chemical feedstocks, lubricants, solvents, and waxes.17 For example, asphalt and road oil are used for roofing and paving, and hydrocarbon gas liquids are used to create intermediate products. In the national Inventory, emissions from these NEUs are estimated separately under the Carbon Emitted and Stored in Products from NEUs source category. Therefore, the amount of fuels used for nonenergy purposes needs to be subtracted from fuel consumption data for determining combustion emissions. The adjustments vary over time and represent about 25% to 30% of total unadjusted industrial sector energy use, as shown in Figure 2-10.

\begin{figure}
\includegraphics[width=1\linewidth]{state_ffc_final_report_files/figure-latex/plots10-1} \caption{Figure 2-10. Transportation Sector State-Level Allocation Examples.}\label{fig:plots10}
\end{figure}

Adjustments for each fuel type were made at the national level based on data and assumptions from EIA as used in the national energy balance. More detail on the amount and types of fuels used for NEU at the national level are shown in Appendix A in the "National 2021 NEU CO2" Tab. The following approaches were taken to determine the amounts of different fuels used for NEUs that needed to be subtracted from energy combustion estimates at the state level. The subtractions were all made in the industrial sector except for lubricants; those subtractions were used in both the industrial and transportation sectors and for NEU from territories. The fuels requiring subtraction are:

• Coking coal. As per the national Inventory, the amount of coking coal used for NEUs was determined to be the total of the adjusted coking coal (after subtracting for IPPU use, per Step 2). Therefore, the state-level totals from Step 2 for coking coal were used to represent NEUs. Appendix A, Table A-59 in the "NEU" Tab, shows this state-level breakout.

• Other coal. The coal used to produce synthetic natural gas at the Eastman gas plant (based on data from the national Inventory) was assumed to be used for chemical feedstock and therefore was accounted for under NEU. This other coal NEU was allocated across states by assuming it all occurred in Tennessee, the location of the Eastman facility. Appendix A, Table A-60 in the "NEU" Tab, shows this state-level breakout.

• Natural gas. The total national-level amount of natural gas used for NEUs was taken from the national Inventory (based on data from EIA) and represents natural gas used for chemical plants and other uses. Natural gas used for NEUs was allocated across states based on the percentage of petrochemical emissions per state. This is an area where there was not any specific data on natural gas used for NEU in chemical plants and other uses by state. Using petrochemical emissions to allocate natural gas NEU use by state was considered a reasonable approach as emissions are a good indication of petrochemical production in a state, and therefore a good indication of how much NEU fuel was used in that state. Petrochemical emissions per state were taken from the IPPU breakout for petrochemicals, as described in Section 3.2.9, and the total percentage for all petrochemicals was used. Appendix A, Table A-61 in the "NEU" Tab, shows this state-level breakout.

• LPG, pentanes plus, still gas, and petroleum coke. The national-level amount of each of these fuels used for NEUs was taken from the national Inventory (from EIA data) and assumed to be used primarily as chemical feedstocks. The amount of NEUs for each fuel was allocated across states based on the percentage of each total fuel use in the industrial sector per the original state-level data from SEDS. The SEDS data includes NEU and fuel combustion uses of fuel so this approach assumes that the percentage of these fuel products used in NEU applications per state are proportional to the fuel combustion uses of these fuel products in a given state. This assumption was considered reasonable as the fuel combustion and NEU applications of these fuel products are likely to be in the same types of chemical facilities. Appendix A, Tables A-63 through A-65 and Tables A-69 through A-72 in the "NEU" Tab, show these state-level breakouts.

• Distillate fuel. The total national-level amount of distillate fuel used for NEUs was taken from the national Inventory (based on data from EIA). Distillate fuel used for NEUs was allocated across states based on the percentage of distillate fuel use in the industrial sector per state after IPPU adjustments described in Step 2. As per the previous group of fuel products, this approach assumed that the percentage of distillate fuel used in NEU applications per state is proportional to fuel combustion uses of distillate fuel in a given state. The national-level data on distillate fuel used in NEU applications are based on industry surveys for nonfuel uses in the chemical industry. Therefore, the assumption that NEUs of distillate fuel are proportional to the total industrial sector amount of distillate fuel use in a given state may not be completely representative because fuel or other uses of distillate fuel in the industrial sector could be very broad. However, it was felt to be a reasonable approach because specific state-level distillate fuel used in NEU applications was not readily available and the percentage of NEUs of distillate fuel was a small fraction of overall industrial sector distillate fuel use (less than 1%). EPA will continue to examine other possible sources for distillate fuel NEU state-level data for future reports. Appendix A, Table A-74 in the "NEU" Tab, shows this state-level breakout.

• Asphalt and road oil, lubricants (in both the industrial and transportation sectors), naphtha (\<401 °F), other oil (\>401 °F), special naphtha, waxes and miscellaneous products. As per the national Inventory, the total amounts of these fuel products were all assumed to be used in NEUs. Therefore, the total state-level data from SEDS were used to represent NEUs for these fuel products. Appendix A, Tables A-62, A-66 through A-68, A-73, and A-75 through A-77 in the "NEU" Tab, show these state-level breakouts.

Emissions associated with NEUs were calculated and reported separately from FFC emissions. Some further adjustments were made to NEU, and carbon factors were applied; see further discussion in Section 2.1.2 below.
