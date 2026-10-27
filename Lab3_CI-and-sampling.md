Geog 4/6300: Lab 3-Confidence intervals
================

## Confidence intervals

**Your name: {Wil Gamble}**

**Overview:**

In this lab, we will create confidence intervals for January
precipitation in Washington during two time periods: 1950-1969 and
2005-2024. Using county-level precipitation data from the ERA5 dataset,
you will estimate statewide precipitation patterns, then examine
variation and change at the county level. We’ve summed the precipitation
in January for each county over two twenty year time periods: 1950-1969
and 2005-2024. You’ll be using those data to create confidence intervals
at a state and county level.

To load these data, load the csv file, which is included in the data
folder:

``` r
era5_data<-read_csv("data/precip_uswest_era5.csv")
```

This is a long format dataset including county identifiers, county
population, median household income, and poverty rate. Census figures
are based on the 2018-22 ACS. The last two columns (year, prcp_m,
time_range) give the year and total precipitation in meters from the
ERA5 NetCDF file, and two time ranges (1950-1969 and 2005-2024).

The script used to create this dataframe is also provided in this
repository (‘lab3_dataprep’), along with rasters of the yearly mean
total precipitation for Januaries in the two time periods of interest.

For this lab to be marked complete, it must demonstrate the following
learning standards:

- 11. Create effective data visualizations across multiple chart types
      (line, scatter, histogram, Q-Q plot), applying appropriate
      aesthetic choices such as color, grouping, and labeling. (Tasks 2
      and 5)
- 12. Produce and interpret confidence interval plots that enable visual
      comparison of means and uncertainty across groups or time periods.
      (Tasks 2, 3, 5, and 6)
- 13. Visualize spatial data using appropriate mapping tools, selecting
      suitable classification schemes and cartographic elements to
      support clear communication. (Task 7)
- 14. Arrange and present tabular results in a readable format
      appropriate for a technical document or report. (Tasks 1 and 4)
- 15. Use measures of central tendency and dispersion to describe the
      variables in tabular datasets (Tasks 1 and 4)
- 20. Explaining how evidence from a sample supports conclusions about a
      population while acknowledging sampling variability and
      uncertainty. (Tasks 3 and 6)
- 22. Construct and interpret confidence intervals, explaining how
      factors such as sample size and spatial scale affect the precision
      of an estimate. (Tasks 1, 3, 4 and 6)

# Part 1: Calculating statewide confidence intervals for Washington’s precipitation

In the first part of this lab, you’ll determine a confidence interval
for Washington’s precipitation for these two time periods. This can be
helpful in identifying whether precipitation has changed across the
state (e.g., whether the two confidence intervals overlap) using the
county data as your sample.

**Task 1** *Load the data and filter it to just include data for the
state of Washington. Then create a table showing mean precipitation,
standard deviation, number of observations (sample size), and
upper/lower confidence interval bounds for each of the two time periods
in the `time_range` field. Use `kable` to display the table when you are
done.*

``` r
Filtered_Washinton_era<-era5_data%>%
  filter(st_name=="Washington")



A_Washington_table<-Filtered_Washinton_era%>%
  group_by(time_range)%>%
  summarise(precipitation_mean=mean(prcp_m),
            precipitation_sd=sd(prcp_m),
            Sample_Size=n(),
            se=precipitation_sd/sqrt(Sample_Size))

z.score_upper=qnorm(0.975)
z.score_lower=qnorm(0.025)

B_Washington_table<-A_Washington_table%>% #could just combine but not how I did it
  mutate(error=z.score_upper*se,
         ci_high=precipitation_mean+error,
         ci_low=precipitation_mean-error)

  kable(B_Washington_table)
```

| time_range | precipitation_mean | precipitation_sd | Sample_Size | se | error | ci_high | ci_low |
|:---|---:|---:|---:|---:|---:|---:|---:|
| 1950-69 | 0.2032239 | 0.1562948 | 780 | 0.0055963 | 0.0109685 | 0.2141923 | 0.1922554 |
| 2005-24 | 0.1664626 | 0.1230138 | 780 | 0.0044046 | 0.0086329 | 0.1750955 | 0.1578298 |

Using the code provided in class, create a ggplot graphic that
visualizes these two confidence intervals. You’ll want to use
`geom_point` and `geom_linerange` to do so. Use states as the y-axis and
mean precipitation as the x axis. Vary the color and position of the
dots and lines for the mean/CI by the `time_range` variable. Feel free
to also experiment with axis labels/themes to make the graph more
appealing and accessible.

**Task 2** *Create a ggplot graphic as described above to show the
confidence intervals for precipitation in Washington for each time
range.*

``` r
ggplot(B_Washington_table%>%mutate(time_range=reorder(time_range,precipitation_mean)))+
  geom_linerange(aes(xmin=ci_low,xmax=ci_high,y=time_range,group=time_range)) +
  geom_point(aes(x=precipitation_mean,y=time_range))
```

![](Lab3_CI-and-sampling_files/figure-gfm/Task3-1.png)<!-- -->

**Task 3** *Interpret your table and graph. What do they tell us about
the change in Washington’s precipitation during this time?*

{Precipitation rates have gone down overall even when looking at the
confidence intervals. 05-24 is about 20% lower in precepition daily than
50-69. 05-24 had a tighter confidence interval, however. There must have
been more variance in 1959-69.}

# Part 2: Analyzing county data in Washington.

In part 1, you ran a state level analysis. What if you wanted to
identify where populations might be negatively affected by precipitation
changes within a state? In this section, you’ll look at the changes in
precipitation **at county level** in Washington to

To start with, adapt your code from step 1 so that you create confidence
intervals for **counties** instead of states. After creating that
dataset, create another ggplot graph of those confidence intervals that
you can compare to the state level data in Task 2 to examine how
precipitation has changed across counties in Washington.

**Task 4** *Create a summary table for January precipitation in
Washington counties for both time periods. Use the same approach as in
Task 1, but calculate statistics for counties rather than the state as a
whole. Call the head of your table with `kable` when you are done.*

``` r
Washington_counties<-Filtered_Washinton_era%>%
  group_by(cty_name,time_range)%>%
  summarise(precipitation_mean=mean(prcp_m),
            precipitation_sd=sd(prcp_m),
            Sample_Size=n(),
            se=precipitation_sd/sqrt(Sample_Size))%>%
   mutate(error=z.score_upper*se,
         ci_high=precipitation_mean+error,
         ci_low=precipitation_mean-error)
```

    ## `summarise()` has regrouped the output.
    ## ℹ Summaries were computed grouped by cty_name and time_range.
    ## ℹ Output is grouped by cty_name.
    ## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
    ## ℹ Use `summarise(.by = c(cty_name, time_range))` for per-operation grouping
    ##   (`?dplyr::dplyr_by`) instead.

``` r
kable(Washington_counties)
```

| cty_name | time_range | precipitation_mean | precipitation_sd | Sample_Size | se | error | ci_high | ci_low |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|
| Adams County, Washington | 1950-69 | 0.0561916 | 0.0275746 | 20 | 0.0061659 | 0.0120849 | 0.0682765 | 0.0441067 |
| Adams County, Washington | 2005-24 | 0.0455177 | 0.0183023 | 20 | 0.0040925 | 0.0080212 | 0.0535389 | 0.0374965 |
| Asotin County, Washington | 1950-69 | 0.0917109 | 0.0412774 | 20 | 0.0092299 | 0.0180903 | 0.1098012 | 0.0736206 |
| Asotin County, Washington | 2005-24 | 0.0661066 | 0.0238369 | 20 | 0.0053301 | 0.0104468 | 0.0765534 | 0.0556598 |
| Benton County, Washington | 1950-69 | 0.0439175 | 0.0199657 | 20 | 0.0044645 | 0.0087502 | 0.0526677 | 0.0351673 |
| Benton County, Washington | 2005-24 | 0.0377814 | 0.0152653 | 20 | 0.0034134 | 0.0066902 | 0.0444716 | 0.0310912 |
| Chelan County, Washington | 1950-69 | 0.2184237 | 0.0983827 | 20 | 0.0219991 | 0.0431173 | 0.2615410 | 0.1753063 |
| Chelan County, Washington | 2005-24 | 0.1817817 | 0.0613328 | 20 | 0.0137144 | 0.0268798 | 0.2086614 | 0.1549019 |
| Clallam County, Washington | 1950-69 | 0.3252100 | 0.1331571 | 20 | 0.0297748 | 0.0583576 | 0.3835676 | 0.2668524 |
| Clallam County, Washington | 2005-24 | 0.2724353 | 0.0979751 | 20 | 0.0219079 | 0.0429387 | 0.3153740 | 0.2294966 |
| Clark County, Washington | 1950-69 | 0.3545254 | 0.1621728 | 20 | 0.0362629 | 0.0710741 | 0.4255995 | 0.2834514 |
| Clark County, Washington | 2005-24 | 0.2886253 | 0.1206821 | 20 | 0.0269853 | 0.0528903 | 0.3415156 | 0.2357350 |
| Columbia County, Washington | 1950-69 | 0.1004024 | 0.0475038 | 20 | 0.0106222 | 0.0208191 | 0.1212215 | 0.0795833 |
| Columbia County, Washington | 2005-24 | 0.0771332 | 0.0302954 | 20 | 0.0067743 | 0.0132773 | 0.0904105 | 0.0638559 |
| Cowlitz County, Washington | 1950-69 | 0.3290205 | 0.1501020 | 20 | 0.0335638 | 0.0657839 | 0.3948044 | 0.2632366 |
| Cowlitz County, Washington | 2005-24 | 0.2670517 | 0.1120230 | 20 | 0.0250491 | 0.0490953 | 0.3161470 | 0.2179564 |
| Douglas County, Washington | 1950-69 | 0.0755475 | 0.0305795 | 20 | 0.0068378 | 0.0134018 | 0.0889493 | 0.0621457 |
| Douglas County, Washington | 2005-24 | 0.0677107 | 0.0237489 | 20 | 0.0053104 | 0.0104082 | 0.0781189 | 0.0573025 |
| Ferry County, Washington | 1950-69 | 0.0872902 | 0.0340067 | 20 | 0.0076041 | 0.0149038 | 0.1021940 | 0.0723864 |
| Ferry County, Washington | 2005-24 | 0.0690214 | 0.0318703 | 20 | 0.0071264 | 0.0139675 | 0.0829890 | 0.0550539 |
| Franklin County, Washington | 1950-69 | 0.0471717 | 0.0234902 | 20 | 0.0052526 | 0.0102948 | 0.0574665 | 0.0368769 |
| Franklin County, Washington | 2005-24 | 0.0396481 | 0.0162463 | 20 | 0.0036328 | 0.0071201 | 0.0467683 | 0.0325280 |
| Garfield County, Washington | 1950-69 | 0.0911108 | 0.0432525 | 20 | 0.0096716 | 0.0189559 | 0.1100667 | 0.0721550 |
| Garfield County, Washington | 2005-24 | 0.0695885 | 0.0257205 | 20 | 0.0057513 | 0.0112723 | 0.0808609 | 0.0583162 |
| Grant County, Washington | 1950-69 | 0.0515536 | 0.0214117 | 20 | 0.0047878 | 0.0093839 | 0.0609376 | 0.0421697 |
| Grant County, Washington | 2005-24 | 0.0452047 | 0.0166150 | 20 | 0.0037152 | 0.0072817 | 0.0524865 | 0.0379230 |
| Grays Harbor County, Washington | 1950-69 | 0.3744625 | 0.1624178 | 20 | 0.0363177 | 0.0711814 | 0.4456439 | 0.3032810 |
| Grays Harbor County, Washington | 2005-24 | 0.3134319 | 0.1198127 | 20 | 0.0267909 | 0.0525093 | 0.3659412 | 0.2609226 |
| Island County, Washington | 1950-69 | 0.2155585 | 0.0854027 | 20 | 0.0190966 | 0.0374287 | 0.2529872 | 0.1781298 |
| Island County, Washington | 2005-24 | 0.1810308 | 0.0653799 | 20 | 0.0146194 | 0.0286535 | 0.2096843 | 0.1523773 |
| Jefferson County, Washington | 1950-69 | 0.3949528 | 0.1741901 | 20 | 0.0389501 | 0.0763408 | 0.4712936 | 0.3186120 |
| Jefferson County, Washington | 2005-24 | 0.3272575 | 0.1196150 | 20 | 0.0267467 | 0.0524226 | 0.3796801 | 0.2748349 |
| King County, Washington | 1950-69 | 0.3027204 | 0.1330477 | 20 | 0.0297504 | 0.0583096 | 0.3610300 | 0.2444107 |
| King County, Washington | 2005-24 | 0.2557177 | 0.0919941 | 20 | 0.0205705 | 0.0403175 | 0.2960351 | 0.2154002 |
| Kitsap County, Washington | 1950-69 | 0.2298190 | 0.0985732 | 20 | 0.0220416 | 0.0432008 | 0.2730198 | 0.1866182 |
| Kitsap County, Washington | 2005-24 | 0.1797918 | 0.0671085 | 20 | 0.0150059 | 0.0294111 | 0.2092028 | 0.1503807 |
| Kittitas County, Washington | 1950-69 | 0.1483310 | 0.0667248 | 20 | 0.0149201 | 0.0292429 | 0.1775739 | 0.1190881 |
| Kittitas County, Washington | 2005-24 | 0.1245704 | 0.0403266 | 20 | 0.0090173 | 0.0176736 | 0.1422440 | 0.1068968 |
| Klickitat County, Washington | 1950-69 | 0.1327846 | 0.0603296 | 20 | 0.0134901 | 0.0264401 | 0.1592247 | 0.1063444 |
| Klickitat County, Washington | 2005-24 | 0.1060991 | 0.0408844 | 20 | 0.0091420 | 0.0179180 | 0.1240171 | 0.0881811 |
| Lewis County, Washington | 1950-69 | 0.2980728 | 0.1346273 | 20 | 0.0301036 | 0.0590019 | 0.3570747 | 0.2390708 |
| Lewis County, Washington | 2005-24 | 0.2437228 | 0.0952494 | 20 | 0.0212984 | 0.0417441 | 0.2854669 | 0.2019787 |
| Lincoln County, Washington | 1950-69 | 0.0690049 | 0.0296052 | 20 | 0.0066199 | 0.0129748 | 0.0819797 | 0.0560301 |
| Lincoln County, Washington | 2005-24 | 0.0547771 | 0.0232163 | 20 | 0.0051913 | 0.0101748 | 0.0649519 | 0.0446023 |
| Mason County, Washington | 1950-69 | 0.3667742 | 0.1645045 | 20 | 0.0367843 | 0.0720959 | 0.4388701 | 0.2946782 |
| Mason County, Washington | 2005-24 | 0.2940343 | 0.1157409 | 20 | 0.0258805 | 0.0507248 | 0.3447590 | 0.2433095 |
| Okanogan County, Washington | 1950-69 | 0.1151731 | 0.0492290 | 20 | 0.0110079 | 0.0215751 | 0.1367483 | 0.0935980 |
| Okanogan County, Washington | 2005-24 | 0.0949922 | 0.0305825 | 20 | 0.0068385 | 0.0134031 | 0.1083953 | 0.0815890 |
| Pacific County, Washington | 1950-69 | 0.2992748 | 0.1259111 | 20 | 0.0281546 | 0.0551820 | 0.3544568 | 0.2440928 |
| Pacific County, Washington | 2005-24 | 0.2460436 | 0.1056389 | 20 | 0.0236216 | 0.0462974 | 0.2923410 | 0.1997461 |
| Pend Oreille County, Washington | 1950-69 | 0.1303871 | 0.0546806 | 20 | 0.0122270 | 0.0239644 | 0.1543515 | 0.1064227 |
| Pend Oreille County, Washington | 2005-24 | 0.0978301 | 0.0429671 | 20 | 0.0096077 | 0.0188308 | 0.1166610 | 0.0789993 |
| Pierce County, Washington | 1950-69 | 0.2933901 | 0.1318172 | 20 | 0.0294752 | 0.0577704 | 0.3511605 | 0.2356197 |
| Pierce County, Washington | 2005-24 | 0.2421974 | 0.0893287 | 20 | 0.0199745 | 0.0391493 | 0.2813467 | 0.2030481 |
| San Juan County, Washington | 1950-69 | 0.2023890 | 0.0737337 | 20 | 0.0164874 | 0.0323146 | 0.2347036 | 0.1700743 |
| San Juan County, Washington | 2005-24 | 0.1744785 | 0.0564386 | 20 | 0.0126200 | 0.0247348 | 0.1992134 | 0.1497437 |
| Skagit County, Washington | 1950-69 | 0.3644765 | 0.1521314 | 20 | 0.0340176 | 0.0666733 | 0.4311498 | 0.2978032 |
| Skagit County, Washington | 2005-24 | 0.3067599 | 0.1106866 | 20 | 0.0247503 | 0.0485096 | 0.3552695 | 0.2582502 |
| Skamania County, Washington | 1950-69 | 0.3942506 | 0.1831491 | 20 | 0.0409534 | 0.0802671 | 0.4745178 | 0.3139835 |
| Skamania County, Washington | 2005-24 | 0.3103656 | 0.1283386 | 20 | 0.0286974 | 0.0562458 | 0.3666114 | 0.2541197 |
| Snohomish County, Washington | 1950-69 | 0.3658901 | 0.1571137 | 20 | 0.0351317 | 0.0688569 | 0.4347469 | 0.2970332 |
| Snohomish County, Washington | 2005-24 | 0.3123520 | 0.1152938 | 20 | 0.0257805 | 0.0505288 | 0.3628808 | 0.2618232 |
| Spokane County, Washington | 1950-69 | 0.0983015 | 0.0448093 | 20 | 0.0100197 | 0.0196382 | 0.1179397 | 0.0786633 |
| Spokane County, Washington | 2005-24 | 0.0761750 | 0.0312343 | 20 | 0.0069842 | 0.0136888 | 0.0898638 | 0.0624862 |
| Stevens County, Washington | 1950-69 | 0.0974383 | 0.0371358 | 20 | 0.0083038 | 0.0162752 | 0.1137135 | 0.0811631 |
| Stevens County, Washington | 2005-24 | 0.0744463 | 0.0326261 | 20 | 0.0072954 | 0.0142988 | 0.0887451 | 0.0601475 |
| Thurston County, Washington | 1950-69 | 0.2486569 | 0.1132318 | 20 | 0.0253194 | 0.0496251 | 0.2982820 | 0.1990318 |
| Thurston County, Washington | 2005-24 | 0.2034090 | 0.0827510 | 20 | 0.0185037 | 0.0362666 | 0.2396756 | 0.1671425 |
| Wahkiakum County, Washington | 1950-69 | 0.2836752 | 0.1204220 | 20 | 0.0269272 | 0.0527763 | 0.3364515 | 0.2308989 |
| Wahkiakum County, Washington | 2005-24 | 0.2261165 | 0.0975296 | 20 | 0.0218083 | 0.0427434 | 0.2688600 | 0.1833731 |
| Walla Walla County, Washington | 1950-69 | 0.0673853 | 0.0326224 | 20 | 0.0072946 | 0.0142971 | 0.0816825 | 0.0530882 |
| Walla Walla County, Washington | 2005-24 | 0.0542915 | 0.0230428 | 20 | 0.0051525 | 0.0100988 | 0.0643902 | 0.0441927 |
| Whatcom County, Washington | 1950-69 | 0.3371718 | 0.1432597 | 20 | 0.0320338 | 0.0627852 | 0.3999570 | 0.2743866 |
| Whatcom County, Washington | 2005-24 | 0.2825477 | 0.0981786 | 20 | 0.0219534 | 0.0430279 | 0.3255756 | 0.2395198 |
| Whitman County, Washington | 1950-69 | 0.0855892 | 0.0426485 | 20 | 0.0095365 | 0.0186912 | 0.1042804 | 0.0668980 |
| Whitman County, Washington | 2005-24 | 0.0678525 | 0.0250818 | 20 | 0.0056085 | 0.0109924 | 0.0788449 | 0.0568601 |
| Yakima County, Washington | 1950-69 | 0.1377246 | 0.0637040 | 20 | 0.0142446 | 0.0279190 | 0.1656436 | 0.1098056 |
| Yakima County, Washington | 2005-24 | 0.1141443 | 0.0395149 | 20 | 0.0088358 | 0.0173178 | 0.1314622 | 0.0968265 |

**Task 5** *Visualize the confidence intervals for the county-level data
you generated in task 4, using the same style as you did for Task 2
above. For an optional extra challenge, visually highlight the counties
where the confidence intervals between time periods do NOT overlap.*

``` r
#You want the y axis to be counties

ggplot(Washington_counties)+ #I tried to mutate this to get it in order of least to most, but I failed. 
  geom_linerange(aes(xmin=ci_low,xmax=ci_high,y=cty_name,group=cty_name))+
  geom_point(aes(x=precipitation_mean,y=cty_name))+
geom_point(aes(x = precipitation_mean, y = cty_name,#I asked chatGPT how to add some color to this
 color = time_range))
```

![](Lab3_CI-and-sampling_files/figure-gfm/Task5-1.png)<!-- -->

**Task 6**: *Compare the size of the error terms for your county level
data to the error terms at state level that you calculated in Task 1.
Are they bigger or smaller? Explain why the formula you used for
calculating the error terms produced error terms of different sizes. How
is this difference visible in your ggplot graphs?*

{Some of the error terms within the counties are much larger than
comparing the statewide trend. Mason County, for example, has an error
close to 0.2. The statewide is within 0.02. The main reason the error is
much greater in the county data is the number of observations. As
observations go up, error decreases. MOE=1.96\*(standard deviation/sqrt
of observations). The difference between counties came from the
differences in standard deviation. Some counties had wide gaps from the
mean making the error larger. Adams county was centered around the mean,
while Mason had a lot of variance.}

**Task 7**: *Transform the county data from task 4 into wide format so
that the means for each time period are in the same row for each county.
Then calculate the change in precipitation between the time periods.
Join this table to the geopackage file that contains county boundaries
and census data, making sure that you only have Washington counties
included - use the geopackage `data/acsdata_cty_westreg.gpkg`. Then use
`tmap` to map out the difference in precipitation means across the two
time periods. Make sure the legend of this map is interpretable.*

``` r
CBWashington_Counties_Wide<-Washington_counties%>%
  select(precipitation_mean,time_range,cty_name)%>%
  pivot_wider(names_from=time_range, 
              values_from=precipitation_mean)%>%
  mutate(precipitation_change=`1950-69`-`2005-24`)

CBWashington_Counties_Wide<-CBWashington_Counties_Wide%>%
  rename(`1950-69_prc_mean`=`1950-69`,
         `2005-2024_prc_mean`=`2005-24`)#Asked chatGPT for the rename function. 

ACS_dat<-st_read("data/acsdata_cty_westreg.gpkg")%>%
  st_transform(4326)%>%
  filter(st_name=="Washington")
```

    ## Reading layer `acsdata_cty_westreg' from data source 
    ##   `C:\Users\wilga\Downloads\Lab_3_Geog4300\Lab3_Person_WG\data\acsdata_cty_westreg.gpkg' 
    ##   using driver `GPKG'
    ## Simple feature collection with 414 features and 8 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -124.7322 ymin: 31.33223 xmax: -102.0416 ymax: 49.00237
    ## Geodetic CRS:  WGS 84

``` r
ACS_Washington<-ACS_dat%>%
  left_join(CBWashington_Counties_Wide,by="cty_name")

tm_shape(ACS_Washington)+
  tm_polygons(fill="precipitation_change",
    fill.scale=tm_scale_intervals(style = "jenks"),
   fill.legend = tm_legend(title = "Difference in Mean precipitation (M): 1959-69 to 2005-24"))
```

![](Lab3_CI-and-sampling_files/figure-gfm/Task7-1.png)<!-- -->

# Optional challenge section: Interpreting the implications of precipitation changes

Now that you’ve calculated these means and confidence intervals, let’s
identify who might be most affected by changes in precipitation. To see
who is affected, create a dummy variable that flags any counties with a
decline of 5 centimeters or more (0.05 meters) with a 1, labeling all
other counties with a 0. Use group_by and summarise to do the following:

- Count the total population in each of these two county groups.
- Calculate the mean value for median income and poverty rate in these
  two groups.

**Task 8** *Create a dummy variable for counties with a change of 0.05
meters or more between the time periods. Calculate how many people
currently live in these counties along with the mean value of median
income (medinc) and poverty rate (pov_pop_pct) for those areas.*

**Task 9** *Create a one paragraph summary of your results from tasks 7
and 8. What do you learn about how precipitation has changed in the last
70 years across the state of Washington? How would you describe the
population affected by large drops in precipitation based on your
results?*

{Your response here.}

Small multiples or multi-panel maps are often a helpful way to show
change over time. See this page for more information about how these are
created in `tmap`:

- <https://r-tmap.github.io/tmap/articles/basics_facets>

Using these tools, create a small multiples map of your precipitation
data for counties in a state of your choice for all 40 years in these
two time periods. This means you will have one figure with 40 individual
maps, all of the same state in different years. These maps should allow
easy comparison across years and should be clearly labeled so that they
could be interpreted by someone with no knowledge of this assignment.

In addition, describe two interesting trends you see in the figure you
created.

{Your response here.}

# Final submission stuff

**Disclosure of assistance:** Besides class materials, what other
sources of assistance did you use while completing this lab? These can
include input from classmates, relevant material identified through web
searches (e.g., Stack Overflow), or assistance from ChatGPT or other AI
tools. How did these sources support your own learning in completing
this lab?

{I mostly used chatGPT for adding clarity to things in this lab. I used
it to add color to the graph in task 4 for clarity. I also asked for the
rename function. I am pretty sure another script covered rename, so I
asked for a template. I actually tried to use it in task 4 to re-order
the counties from most to least error using mutate. My mutate worked
originally when I did not include the years. I tried altering the code
by doing a second mutate using the years. That did not work, and I tried
chatGPT. It also failed. }

**Lab reflection:** How do you feel about the work you did on this lab?
Was it easy, moderate, or hard? What are the biggest things you learned
by completing it?

{This lab followed very closely to script 15. The first 6 task were not
bad at all. I had some trouble with task 7, but I mainly relied on
script 10 to get through it. The whole joining section was based around
code from 10. You can see from my naming, that it took me multiple
iterations of “Wide_Washington” to be happy. I originally had an
extremely round about way with new mutated columns that was not needed.
I mainly learned about confidence intervals from class and the lab. I
understand standard deviations, showing confidence with error bars, and
how the mean matters for these now. I see how observations lowers error
as well. I can interpret graphs better now for error. I see error bars a
lot in herbicide efficasy graphs so this clarifies how they find them.
“Big error” is not necissarily a bad thing. Could be from a small trial
or their trial plants have a varried response.}
