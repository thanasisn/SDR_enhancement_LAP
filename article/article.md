---
title: Analysis of cloud enhancement events in a 30-year record of global solar irradiance at Thessaloniki, Greece

author:
  - name:        Athanasios N. Natsis
    email:       natsisphysicist@gmail.com
    affiliation: LAP
    correspondingauthor: true
    # footnote: 1
  - name:        Alkiviadis Bais
    email:       abais@auth.gr
    affiliation: LAP
  - name:        Charikleia Meleti
    email:       meleti@auth.gr
    affiliation: LAP
    # footnote: 1
address:
  - code:         LAP
    organization: Laboratory of Atmospheric Physics, Physics Department, Aristotle University of Thessaloniki
    # addressline:  1 main street
    city:         Thessaloniki
    postcode:     54124
    country:      Greece
  # - code:         Another University
  #   organization: Department
  #   addressline:  A street 29
  #   postcode:     2054 NX
  #   city:         Manchester,
  #   state:        State
  #   country:      The Netherlands
# footnote:
#   - code: 1
#     text: "This is the first author footnote."
#   - code: 2
#     text: "Another author footnote."
abstract: |
  In this study we investigate the characteristics of global horizontal irradiance
  enhancement events induced by clouds over Thessaloniki for the period 1993 -- 2023
  using data recorded every one minute. We identified the cloud enhancement (CE)
  events by creating an appropriate clear-sky irradiance reference with the use of a
  radiative transfer model and aerosol optical depth data from a collocated Cimel sun
  photometer and a Brewer spectrophotometer. We found a trend in CE events of
  \(45.6\pm 21.9\,\text{cases}/\text{year}\), and a trend in the CE events
  irradiation of \(116.9\pm 67.8\,\text{kJ}/\text{year}\). The peak of the CE events
  was observed during May and June. The analysis of the total duration of CE events
  showed that durations longer than 5 minutes are very rare, with exceptions lasting
  over an hour and up to 140 minutes. Finally, we have detected enhancements above
  the total solar irradiance at the top of the atmosphere of up to
  \(400\,\text{W}/\text{m}^{2}\), with the \(75\,\%\) of the cases below
  \(200\,\text{W}/\text{m}^{2}\). The most active period of these extreme events is
  spring -- early summer with a secondary peak in autumn.

header-includes:
  - \usepackage{caption}
  - \usepackage{placeins}
  - \captionsetup{font=small}
  - \usepackage{subcaption}

keywords:
  - cloud enhancement
  - total solar radiation
  - global horizontal irradiance
  - over irradiance

journal:        "Atmospheric Research"
date:           "2024-08-31"
linenumbers:    false
numbersections: true
bibliography:   bibliography.bib

biblio-style:   elsarticle-num-names # elsarticle-harv author year style for natbib - use 'elsarticle-num' or 'elsarticle-num-names' for numbered scheme

classoption:    preprint, 5p, authoryear # remove authoryear is not using `elsarticle-harv`
# Use a CSL with `citation_package = "default"`
# csl: https://www.zotero.org/styles/elsevier-harvard


output: 
  rticles::elsevier_article:
    keep_tex:         true
    keep_md:          true
    citation_package: natbib
  bookdown::pdf_document2:
    number_sections:  yes
    fig_caption:      yes
    keep_tex:         yes
    latex_engine:     xelatex
    toc:              yes
    toc_depth:        4
    fig_width:        7
    fig_height:       4.5
    documentclass:    article
    classoption:      a4paper,oneside
    fontsize:         12pt
    geometry:         "left=0.75in,right=0.75in,top=0.75in,bottom=0.75in"
    link-citations:   yes
    colorlinks:       yes
    urlcolor:         blue
    biblio-style:     apalike
  bookdown::word_document2: default
---








# Abstract {.unnumbered} 

**Abstract temporary placed here for technical reasons**

We studied the enhancement of the global horizontal irradiance induced by clouds over
Thessaloniki for the period 1993 -- 2023 using data recorded every one minute.  We
identified the cloud enhancement events (CE) by creating an appropriate clear sky
irradiance reference with the use of a radiative transfer model and aerosol optical
depth data from a collocated Cimel sun photometer and a Brewer spectrophotometer. We
found a trend in CE events of
$46\pm
22\,\text{cases}/\text{year}$,
and a trend in the CE events irradiation of 
$116.3\pm
68.2\,\text{kJ}/\text{year}$.
The peak of the CE events was observed during May and June.  An analysis of the total
duration of CE events, showed that durations longer than 5 minutes are very rare,
with exceptions lasting over an hour and up to 140 minutes.  Finally, we have
detected enhancements above the total solar irradiance at the top of the atmosphere
for the same solar zenith angle of up to
$400\,\text{W}/\text{m}^{2}$, with the $75\,\%$ of the cases below
$200\,\text{W}/\text{m}^{2}$. The most active season of these extreme events is
spring until early summer and autumn. 


<!--

$\text{GHI}_\text{i}$: measured one minute global horizontal irradiance 

$\text{GHI}_\text{CSm}$: modelled clear sky one minute global horizontal irradiance 
-->


# Introduction

The shortwave solar radiation, reaching Earth's surface, is the main energy source
for the atmosphere and the biosphere, and drives and governs the climate [@Gray2010].
It has direct practical application in industries related to energy and agricultural
production. The variability of its intensity can impose difficulties in predicting
the yield and in designing the specifications of the appropriate equipment.
Significant portion of research has been focused on predicting the renewable energy
production in a fine timescale and in near real-time [for a review see @Inman2013;
@Graabak2016].

An important aspect of the variability of solar radiation is its interaction with the
clouds. In general, clouds can attenuate a fraction of solar irradiance, but under
certain conditions, can lead to enhancement of the global horizontal irradiance (GHI)
reaching the ground. This cloud enhancement (CE) effect can locally increase the
observed GHI to levels even higher than the expected clear-sky irradiance
[@Cordero2023; @Vamvakas2020; @CastillejoCuberos2020;  and references
therein].

Some of the proposed underling mechanisms of those enhancements, have been summarized
by @Gueymard2017; the most important being the scattering of radiation on the edges
of cumulus clouds.  It has been also suggested that enhancement of GHI can be
produced by thin cirrus clouds though refraction and scattering [@Thuillier2013].
Further investigation with radiative transfer modeling and observations pointed as
the prevailing mechanism, the strong forward Mie scattering through clouds of low
optical depth [@Pecenak2016; @Thuillier2013; @Yordanov2013; @Yordanov2015].  Overall,
enhancement events depend on different interactive factors, which include cloud
thickness, structure and type, and the relative position of the sun and the clouds
[@Gueymard2017; @Veerman2022].

On multiple sites, cloud enhancements have been reported that exceed momentarily the
solar constant, resulting in clearness indices above unit. A summary of extreme cloud
enhancement (ECE) cases has been compiled by @Martins2022. Cloud enhancements can
have also some practical implications.  The intensity and duration of enhancements
can affect the efficiency and stability of photovoltaic power production
[@Lappalainen2020; @Jaervelae2020], while ECEs have the potential to compromise the
integrity of photovoltaic plants infrastructure [@DoNascimento2019].  It has also
been demonstrated that these events can interfere in the comparison of ground-based
and satellite observations of radiation [@Damiani2018].  Global warming has likely
affected cloud coverage in the last few decades. @Liu2023 reported increases in cloud
cover over the tropical and subtropical oceans and decreases over most continents,
while @Dong2023 reported decreases over North America and Europe. To our knowledge,
there is no evidence on whether this trend has affected also the number and strength
of CE events.

<!--
Study of the mechanism has also been done with modeling of different cloud types and
atmospheric conditions.  @Gueymard2017; @Veerman2022

Instruments response ....
@Martins2022
-->

Methods of identification of CE events usually include the use of simulated clear
sky irradiance as baseline, combined with an appropriate threshold or some
other statistical characteristics, and in some cases, with visual inspection of sky
camera images [@Vamvakas2020; @Mol2023 and references therein].

<!-- our work -->
In this study, we evaluate the effects of CE on GHI by investigating their frequency
of occurrence, intensity, and duration in a thirty-year period of GHI observations at
Thessaloniki, Greece.  We used modeled clear sky irradiance as a baseline to identify
cloud enhancements, and we determined long-term trends of the above-mentioned
metrics, their climatology and some general characteristics.  To our knowledge there
are no other studies that provide trends from such long dataset.

<!--
Identification methods from bib

- clearness index
- modeled approaches
- algorithmic id
- AI
- Visual

Constrains of the site

- data
- instrument
- location
-->


# Data and methodology

## Instrumentation and data

The data used in this study were recorded at the monitoring site of the Laboratory of
Atmospheric Physics, Aristotle University of Thessaloniki, in Thessaloniki, Greece
(\(40^\circ\,38'\,\)N, \(22^\circ\,57'\,\)E, \(80\,\)m\ a.s.l.).  The GHI data were
measured with a Kipp\ & Zonen CM-21 pyranometer and cover the period
01\ January 1994 to
31\ December 2023.
<!-- Instrument -->
  During the study
period, the pyranometer was independently calibrated three times at the
Meteorologisches Observatorium Lindenberg, DWD, verifying that the stability of the
instrument’s sensitivity was better than $0.7\,\%$ relative to the initial
calibration by the manufacturer.
<!-- Measurements -->
For the acquisition of radiometric data, the signal of the pyranometer was sampled at
a rate of $1\,\text{Hz}$ with the mean and standard deviation of these samples
calculated and recorded every minute.  The measurements were corrected for the zero
offset ("dark signal" in volts), which was calculated by averaging all measurements
recorded for a period of $3\,\text{h}$, before (morning) or after (evening) the Sun
reaches an elevation angle of $-10^\circ$.  The signal was converted to irradiance
using the ramped value of the instrument’s sensitivity between subsequent
calibrations.

<!-- Radiation data quality check -->
To further improve the quality of the irradiance data, a manual screening was
performed, in order to remove inconsistent and erroneous recordings that can occur
stochastically or systematically during the long operation of the instrument.  The
manual screening was aided by a radiation data quality assurance procedure, adjusted
for the site, which was based on the methods of Long and Shi\ [@Long2006;
@Long2008a].  Thus, problematic recordings have been excluded from further
processing.  Furthermore, due to the significant measurement uncertainty in GHI when
the Sun is near the horizon, and due to some systematic obstructions by nearby
buildings, we have excluded all measurements with solar zenith angle (SZA) greater
than $78^\circ$.  Finally, images from a sky camera have been used in
the manual inspection of the CE identification. The sky camera operates since 2012
and stores images in 5 min time steps.
<!--
Although it is impossible to detect all false data, the large number of
available data, and the aggregation scheme we used, ensures the quality of the
radiation measurements used in this study.
-->
<!-- Data selection -->
<!--
To preserve an unbiased representation of the data we applied a constraint, similar
the one used by @CastillejoCuberos2020. For each valid hour of day, there must exist
at least 45 minutes of valid measurements, including nighttime, when near sunrise and
sunset. Days with less than 5 valid hours were rejected completely.
-->
<!--
# CastillejoCuberos2020 There must be at least 5 h of valid data (Tests 1–7),
otherwise all data for this day is flagged as invalid. There must be at least This
work valid data 45 min of valid data (Tests 1–7) for an hour to be flagged as valid,
otherwise all minute data for this hour is flagged as invalid.
-->


## Cloud enhancement detection

<!-- Define CE and ECE tomove? -->
In this study, we define an event as CE when the measured GHI at ground level,
exceeds the expected value under clear-sky conditions. Similarly, we define as
extreme cloud enhancement events (ECE), the cases when GHI at ground level exceeds
the Total Solar Irradiance (TSI). Although the duration of these bursts can vary from
seconds to several minutes, here we are constrained by the temporal resolution of our
data to identify events with duration of at minimum one-minute.

For the detection of CE cases we established a baseline of irradiance above which we
characterized each data point as an enhancement event and calculated the over
irradiance (OI). The OI is defined as the irradiance difference of the measured
one-minute GHI from the $\text{GHI}_\text{ref}$ corresponding to cloud-free
atmosphere.
<!-- CE identification criterion ($\text{OI}_i =
\text{GHI}_i - \text{GHI}_\text{CSlim}$), as defined in
Equation\nobreakspace\ref{eq:CE4}. -->
First, we used a simple approach for the determination of
$\text{GHI}_\text{ref}$: The Haurwitz's model [@Haurwitz1945], which is a simple
clear sky model and was already adjusted and applied to our data [@Natsis2023].
<!--
, and the total solar irradiance (TSI) at the top of the atmosphere, adjusted for the
Sun-Earth distance.
-->
We created a threshold by using an appropriate relative and/or an additional constant
offset.  The initial results showed that we can detect a big portion of the actual CE
events.  However, by inspecting the daily plots of irradiance it became evident that
changes in atmospheric conditions introduced numerous false positive or false
negative results. The main reason for these discrepancies is the variability of the
effects of aerosols and water vapor which were not taken into account in the two
simple methods.  To produce a more representative reference we included the effects
of these factors using a radiative transfer model (RTM).  The applied methodology is
discussed in section\nobreakspace{}\ref{rtmcs}.

## Modeled clear Sky Irradiance {#rtmcs}

### Climatology of clear sky irradiance

<!-- Libradtran options -->
We approximated the expected clear sky $\text{GHI}_\text{ref}$ with the radiative
transfer model uvspec, part of libRadtran [@Emde2016], similarly to the approach used
by @Vamvakas2020.  In uvspec we used the solar spectrum of @Kurucz1994 in the range
$280$ to $2500\,\text{nm}$, the radiative transfer solver "disort" in
"pseudospherical" geometry and the "LOWTRAN" gas parameterization.  The model was run
for a range of variables in order to create a look-up table (LUT) for the estimation
of the cloud-free reference irradiance for each individual observation of our
dataset. In this context, the model was run for SZAs in the range $10$ -- $90^\circ$
with a step of $0.2^\circ$ and for the atmospheric profiles of the Air Force
Geophysics Laboratory [@Anderson1986] midlatitude summer and midlatitude winter,
representative of the warm and cold seasons.

Main factors responsible for the attenuation of the broadband downward solar
radiation under cloud free atmospheres are aerosols and water vapor.  At
Thessaloniki, such measurements are available since 2003 from the Cimel sun
photometer that belongs to the Aerosol Robotic Network (AERONET) [@Giles2019;
@Buis1998].  From the observations in the period 2003 -- 2023 we calculated the
monthly climatological means and standard deviation ($\sigma$) for the aerosol
optical depth (AOD) at $500\,\text{nm}$ and the equivalent height of the water column
(WC).  The monthly climatological values of AOD and WC, as well as combinations with
additional offsets of $\pm1\sigma$ and $\pm2\sigma$, were used as inputs to the RTM
in the construction of the LUT.

For each measurement of the dataset, we calculated from the LUT a
$\text{GHI}_\text{ref}$ value for the respective season and SZA (by linear
interpolation), and for the climatological values of AOD and WC of the respective
month. The same procedure was followed for the estimation of the
$\text{GHI}_\text{ref}$ for all combinations of the AOD and WC with the
above-mentioned standard deviation offsets.  Finally, each $\text{GHI}_\text{ref}$
value was adjusted to the actual Sun-Earth distance derived by the Astropy software
library [@AstropyCollaboration2022].


<!-- Lookup table -->
<!-- 
To create a look-up table, that aligns with our dataset, we applied some adjustments.
To account for the Sun's variability, in our one-minute GHI measurements, we adapted
each modeled value by scaling the model's input spectrum integral, to the
corresponding TSI, provided by NOAA [@Coddington2005].  Also,
we account for the effect of the Earth -- Sun distance on the irradiance, by using
the distance calculated by the Astropy [@AstropyCollaboration2022] software library.
As needed, we interpolate the resulting irradiances to the SZA of our
measurements.  For each period of the year, we used the appropriate atmospheric
profile (afglms or afglmw).  Finally, we calculated the clear sky irradiance value at
the horizontal plane.  Thus, we were able to emulate different atmospheric condition
and levels of atmospheric clearness for the climatological conditions of the site.
With this method, the modeled clear sky irradiances can be directly compared to each
measured one-minute value of GHI, for different conditions of atmospheric clearness.
-->

### Long-term change of clear sky irradiance

<!-- Aerosol change -->
The clear-sky reference values discussed above are based on the climatological AOD
and WC; hence they cannot describe accurately the long-term variation of
$\text{GHI}_\text{ref}$ due to long-term changes in the two atmospheric constituents,
mainly AOD.  As reported by @Natsis2023, there is a long-term brightening effect in
the GHI data of Thessaloniki for the period 1993 -- 2023, which for clear-sky data
was attributed to long-term changes in aerosol effects.  Therefore, an adjustment of
the $\text{GHI}_\text{ref}$ during the period of study was made using simulations
with the RTM based on the long-term variations of the AOD. As AERONET data start only
in 2003, we used for the period 1993 -- 2005 estimates of changes in AOD at
$340\,\text{nm}$ derived from a collocated Brewer spectrophotometer [@Kazadzis2007]
to calculate the change in $\text{GHI}_\text{ref}$ due to aerosols during this period.

<!--
$-3.8\,\%$ per year at AOD on $340\,\text{nm}$, using a Brewer spectrophotometer. For
-->

<!--
To create a unified trend for the long-term change of the clear sky irradiance, we
simulated the values of AOD derived from those sources with Libradtran. For both
inputs, we used the AOD at $500\,\text{nm}$, which was inferred by the available
Ångström coefficients. We choose the SZA of $55^\circ$ as a representative value for
all the runs.
-->

According to @Kazadzis2007, in the period 1997 -- 2005 the mean AOD at $340\,\text{nm}$
is $0.403$ with a change of $-3.8\pm0.93\,\%$ per year, corresponding to a change of
$0.0153$ per year.  Using an Ångström coefficient $\alpha = 1.6$, this translates to
a change in the Ångström coefficient $\beta$ of $0.00272$ per year (or $\beta=0.084$ in
1997 and $\beta=0.059$ in 2005).
<!--
Simulations with uvspec for the above Ångström
coefficients, with WC of
$15.7\,\text{mm}$ and
$15.4\,\text{mm}$
for 1997 and 2005 respectively, where for 1997 we used the first available WC data
(i.e. 2003) as a proxy, also taken from AERONET, and for a SZA of $55^\circ$ reveal a
change of
$+0.21\,\%$ per year in $\text{GHI}_\text{ref}$. 
-->
Simulations with uvspec for the above Ångström coefficients, assuming constant WC of
$15.6\,\text{mm}$ taken from the Cimel, and for a SZA of $55^\circ$ reveal a change of
$+0.21\,\%$ per year in $\text{GHI}_\text{ref}$.  The SZA of $55^\circ$ was chosen as
representative of all days in the year in order
to get a rough estimate of the annually averaged change in clear sky irradiance.  For
the period 2005 -- 2023 we used the mean monthly values of AOD and WC from AERONET in
a similar simulation scheme to calculate the monthly mean clear-sky irradiance,
and finally the change of $+0.14\,\%$ per year. We applied these two long-term changes
(see Figure\nobreakspace{}\ref{fig:CS-change}) to the climatological
$\text{GHI}_\text{ref}$, in order to create a more realistic representation of the
clear-sky irradiance for the whole period of study.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-CS-change-1} 

}

\caption{Simulated long-term change in clear sky irradiance relative to the climatological values due to changes in AOD in Thessaloniki for the period 1993 -- 2023.}\label{fig:CS-change}
\end{figure}

<!-- 1997 0.0839901
<!-- 2005 0.05496013
<!-- # Kazadzis2007 - Nine years of UV aerosol optical depth measurements at Thessaloniki, Greece
<!-- # From Stelios' paper 2007:
<!-- # AOD @340 from Brewer 086
<!-- # Period: 1997-2005
<!-- # The mean AOD for this period was calculated to 0.403 for the Brewer and 0.422 for the CIMEL.
<!-- # A linear regression on the Brewer deseasonalized data reveals a change of -3.8±0.93% per year.
<!-- # From the above data AOD @340 changes by 0.0153 per year (or 0.138 for the 9 years).
<!-- # Using Angstrom a= 1.6 this translates to a change in Angstrom β=0.00272 per year (or β=0.084 in 1997 and β=0.059 in 2005).
-->


## Criteria for the identification of CE events

In this study our main focus was to quantify the OI related to CEs. A
key issue for achieving this goal is to define a threshold for the CE
identification, representative of the clear-sky irradiance at the time of each GHI
measurement. This depends on the selection of the appropriate atmospheric
parameterization for the RTM simulations. The implementation of the long-term change
in AOD, discussed in section\nobreakspace{}\ref{rtmcs}, allows capturing a large part
of the natural variability of clear-sky GHI. However, the short-term variability of
AOD cannot be taken adequately into account when using monthly values in the model
simulations. We tried different approaches in order to strengthen the robustness of
the methodology and to compensate for the limited accuracy of the RTM input data and
the unpredictable natural variability of the atmosphere.

First, we evaluated the performance of the modelled $\text{GHI}_\text{ref}$ in
relation to the measured GHI for different levels of atmospheric clearness, by using
in the RTM the monthly climatological AOD and WC, less their respective standard
deviations.
<!--
$\tau_{\text{cs}} = \tau_{500\text{nm}} - 1\sigma$ and $w_{h\text{cs}} = w_h - 1\sigma$
($\text{GHI}_\text{CSm}$).
-->
These values represent typical atmospheres in Thessaloniki with lower than average
load of aerosols and humidity, which are the main factors that attenuate the GHI,
excluding clouds.  With this approach the simulated $\text{GHI}_\text{ref}$ should be
generally greater than the measured GHI when aerosols are more abundant.  To
compensate for this, we defined the following threshold $E$ to compare the
measured $\text{GHI}$: 
<!--
($4\,\%$)
with an additional constant offset of
$15\,\text{W}/\text{m}^2$,
as described in Equation\nobreakspace\ref{eq:CE4}.
-->
\begin{equation}
\text{CE} : E > 15 + 1.04 \cdot \text{GHI}_\text{ref} \,\,[\text{W}/\text{m}^2] \label{eq:CE4}
\end{equation}

<!--
where: $\text{E}$ the measured irradiance, $\text{E}_\text{CSm}$ the selected
modelled clear sky irradiance, and $i$ each of the one-minute observation.  This is
the criterion of our CE identification.
-->

This is the criterion of our CE identification.  The constant terms were determined
through the implementation of an empirical method with manual inspection of the CE
identification results on selected days of the whole dataset.  We tested seven sets
of days with different characteristics relevant to the efficiency of the
identification threshold.  These sets were random groups of about 20 -- 30 days with
the following characteristics:
(a) the strongest OI CE events,
(b) the largest daily total OI,
(c) absence of clouds (by implementing a clear sky identification algorithm as discussed in @Natsis2023),
(d) absence of clouds and absence of CE events,
(e) with at least $60\,\%$ of the day length without clouds and presence of CE events,
(h) randomly selected days, and
(i) manually selected days.
For the latter case and where needed, we also used images from the sky-camera to
further aid the decision of the manual inspection.

<!--
\begin{equation}
\text{CE}_\text{Threshold} = \begin{cases}
1.04, & \text{$\theta \leq 90^\circ$}\\
\frac{1.04 - 1.04}{90 - 78} \cdot (\theta- 78) + 1.04 , & \text{$ 78^\circ > \theta > 90^\circ$}\\
\text{Excluded measurements}, & \theta > 78^\circ
\end{cases}\label{eq:CE4a}
\end{equation}
where: $\theta$ solar zenith angle.
-->
<!--
\begin{equation}
\text{CE} : \begin{cases}
 \text{GHI}_\text{i} > 1.04 \cdot \text{GHI}_\text{mCSi}, & \text{$\theta \leq 90^\circ$}\\
\text{GHI}_\text{i} > \left ({ 1.04 + \frac{1.04 - 1.04}{90 - 78} \cdot (\theta- 78) } \right ) \cdot \text{GHI}_\text{mCSi}, & \text{$ 78^\circ > \theta > 90^\circ$}\\
\text{Excluded measurements}, & \theta > 78^\circ
\end{cases}\label{eq:CE4b}
\end{equation}
where: $\theta$ is the solar zenith angle, $\text{GHI}_\text{i}$ the measured
irradiance, and $\text{GHI}_\text{mCSi}$ the selected modelled clear sky irradiance.
-->
<!-- \begin{equation} -->
<!-- \text{CE} : \text{E}_\text{i} > \text{E}_\text{CSlim,i}, \text{where} \begin{cases} -->
<!--  \text{E}_\text{CSlim,i} = 1.04 \cdot \text{E}_\text{CSm,i}, & \text{$\theta \leq 90^\circ$}\\ -->
<!-- \text{E}_\text{CSlim,i} = \left ({ 1.04 + \frac{1.04 - 1.04}{90 - 78} \cdot (\theta- 78) } \right ) \cdot \text{E}_\text{CSm,i}, & \text{$ 78^\circ > \theta > 90^\circ$}\\ -->
<!-- \text{Excluded measurements}, & \theta > 78^\circ -->
<!-- \end{cases}\label{eq:CE4} -->
<!-- \end{equation} -->
<!-- where: $\theta$ is the solar zenith angle, $\text{E}$ the measured -->
<!-- irradiance, $\text{E}_\text{CSm}$ the selected modelled clear sky irradiance, and $i$ each of the one-minute observation. -->


<!-- reasoning for criteria -->
<!-- We have to note, that the differentiation of the threshold factor was needed, because
<!-- of the high irradiance values we observed, early in the morning and late in the
<!-- afternoon. 
<!-- We have confirmed, by inspecting images from the sky cam, that we have
<!-- duration of elevated irradiance, either due to the clearness of the atmosphere, or some
<!-- interferences by reflections on nearby bright surfaces. Although, these cases may
<!-- mask some of the CE events. The actual CE events, produce higher irradiance and thus
<!-- are identified as such. As a side effect, the reported OI will be slight
<!-- underestimated for those SZAs, but the overall contribution of those cases to the
<!-- total daily energy, is minimal due the low occurrences and the lower irradiances.
-->

<!--
- can follow for clear days
- the most important are the thresholds
- model have to be uniform on clear days
- the level is adjusted by the threshold 
- the effect of the sza uniform
-->

The definition of the CE events with this method has a degree of subjectivity, since
the actual clear sky irradiance is not known and can only be approximated.  However,
this method was proven capable in detecting all major CE events.  Where some CE
events with very low OI may be not detected, these are few with small
over-irradiance, and it is unlikely that will affect significantly our results.

<!-- extreme caces criterion -->
A sub-category of the CE events that is often discussed in the literature
[@Cordero2023; @Martins2022; @Yordanov2015], are the extreme cloud enhancement (ECE)
events. These are cases of CE where the measured intensity of the irradiance exceeds
the TSI at the top of the atmosphere. In this case the threshold $E$ is given by:
<!--and detected according to the Equation\nobreakspace{}\ref{eq:ECE}.-->
\begin{equation}
\text{ECE}: E > \cos(\text{SZA}) \cdot E_{\odot} \frac{r^2_\text{m}} {r^2}
\label{eq:ECE}
\end{equation}
where: $\text{SZA}$ the solar zenith angle, $E_{\odot}$ is the solar constant at the
mean Sun - Earth distance ($1361\,\text{W}/\text{m}^2$), $r$ is
the actual Sun - Earth distance and $r_\text{m}$ is the mean Sun -- Earth distance of
$1.496\times10^8\,\text{km}$.

An example of CE identification for a selected day is given in the
Figure\nobreakspace{}\ref{fig:example-day}, where the daily course of the clear sky
reference irradiance and the CE and ECE identification thresholds are shown along
with the actual GHI measurements.  In addition, we provide an example scatter plot
between the measured and the modeled clear-sky irradiance for one year, where the CE
and ECE events are clearly grouped above the threshold of irradiance
(Figure\nobreakspace{}\ref{fig:example-year}).

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../images/example-days-18} 

}

\caption{Example of CE identification in Thessaloniki for 2019-07-11. The green line with blue symbols depicts the measured GHI in one minute steps. Red line shows the modelled threshold $E$ for the detection of CE events, which are denoted with red circles.Black curve represents the TOA TSI on horizontal plane.}\label{fig:example-day}
\end{figure}

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../images/P-example-years-12} 

}

\caption{Example scatter plot of the measured GHI and the reference clear sky irradiance in Thessaloniki for the year 2005. The over-irradiance for CE and ECE events is color coded, while the remaining data points are shown in black. The reference green line goes through the origin with a slope of one.}\label{fig:example-year}
\end{figure}

<!-- ```{r echo=F, include=T, warning=FALSE, message=FALSE} -->
<!-- ST_total -->
<!-- cat("\n - - - \n") -->
<!-- ST_daily[which.max(wattGLB.max), wattGLB.max] -->
<!-- round(ST_E_daily[which.max(GLB_diff.max), GLB_diff.max], 2) -->
<!-- round(100*ST_daily[which.max(GLB_ench.max), GLB_ench.max],1) -->
<!-- plot(ST_daily$Date, ST_daily$GLB_diff.max) -->
<!-- ``` -->


# Results

Following the application of the above discussed methodology to the entire dataset 
($6$ million of one-minute GHI measurements),
$1.78\,\%$ were identified as CE events and
$0.04\,\%$ as ECE events.
The highest recorded GHI due to CE was
$1416.6\,\text{W}/\text{m}^2$ on
 24\ May 2007 at a SZA of
$19.9^\circ$ corresponding to OI of
$345.9\,\text{W}/\text{m}^2$ or
$32.3\,\%$ above the threshold.
The strongest CE event of
$49.7\,\%$ above the 
clear sky threshold was observed on 
28\ October 2016 at a SZA of
$59.2^\circ$ with a GHI value of 
$861.8\,\text{W}/\text{m}^2$ and a OI of
$286.1\,\text{W}/\text{m}^2$.
Both cases are ECE events with
$136.8\,\text{W}/\text{m}^2$ and
$164.5\,\text{W}/\text{m}^2$ above the
TSI at TSI for the same SZA, respectively.
In the following sections we are discussing the long-term trends and variability of
the CE events as well as of the corresponding OI and excess irradiation.


## Long-term trends

One aspect of this study is to investigate the time evolution of the CE events by
analyzing the GHI measurements at Thessaloniki. Cloud enhancements can be influenced
by different factors, such as the geometry, size and optical thickness of clouds,
their height in the atmosphere and local weather regimes [@Mol2023; @Veerman2022;
@Gristey2022; @Tzoumanikas2016].  Some of these factors are related to changes in
climate; hence it would be reasonable to expect contributing to the frequency of
occurrence of CE events over Thessaloniki, as well as to the average OI and excess
irradiation.  The long-term trends were calculated using a first-order autoregressive
model with the 'maximum likelihood' fitting method [@Gardner1980; @Jones1980], by
implementing the function 'arima' from the library 'stats' of the R programming
language [@RCT2023]. All trends are reported together with their $2\sigma$ error.

Figure\nobreakspace{}\ref{fig:P-energy} shows the time series of the yearly number of
CE cases (each with duration of one minute), the yearly mean OI and the yearly
excess irradiation for the period 1993 -- 2023, together with corresponding linear
trends. All three quantities show increasing trends, most pronounced for the
frequency of occurrence 
($+46\pm 22\,\text{cases}/\text{year}$)
and the excess irradiation 
($+116.3 \pm 68.2\,\text{kJ}/\text{year}$),
which are also statistically significant.
In contrast the trend of the yearly mean OI is negligible 
($+0.03\pm 0.1\,\text{W}/\text{m}^2/\text{year}$)
and of no statistical significance.
The average OI for the entire period is
$+40.1\pm 2.5\,\text{W}/\text{m}^2$.
The interannual variability of the data about the trend lines is quite large.
Furthermore, the spread tends to increase with time (at least for the quantities of
panels b and c), suggesting a significant variability in cloud patterns over the
area, possibly associated to changes in climate.

We have to note that the excess irradiation related to the CE events cannot be
directly linked to the total energy balance of the atmosphere. The net solar
radiation of the region is not increased, but is rather redistributed through the CE
events.  This is also depicted by the ECE irradiance values, which exceed the
equivalent clear sky irradiance by a significant amount.

\begin{figure}% [h!]
        {\centering 
            \subfloat[\label{fig:P-energy-mean}]
                {\includegraphics[width=\linewidth]{../images/P-energy-3} }\\
            \subfloat[\label{fig:P-energy-N}]
                {\includegraphics[width=\linewidth]{../images/P-energy-2} }\\
            \subfloat[\label{fig:P-energy-sum}]
                {\includegraphics[width=\linewidth]{../images/P-energy-1} }
        }
    \caption{Time series for the period 1993 -- 2023 of (a) the yearly CE number of occurrences, (b) the yearly mean OI and (c) the yearly excess irradiation. The black lines represent the linear trends on the yearly data.}\label{fig:P-energy}
\end{figure}


## Climatology of cloud enhancement events

Next we investigated the distribution of the CE events within the year.
Figure\nobreakspace{}\ref{fig:relative-month-occurrences} shows the monthly box and
whisker plot of the CE number of occurrences normalized with the highest median
value, which occurs in June, depicting a clear seasonal cycle.  Although CE events
are present throughout the year, the most active months are May and June. During the
winter (December -- February), the number of CE cases is about $25\,\%$ of the
maximum, while in the intermediate months, the number of occurrences gradually ramps
between the maximum and minimum.  This seasonality is a combined effect of different
factors, among them the types of clouds, their frequency of occurrence, the
seasonally varying relative position of the sun, as well as the local landscape
characteristics that may influence the formation of the clouds.  Unfortunately, lack
of detailed data on cloud formation, type and location is not allowing further
analysis. The interannual variability of the monthly CE events is quite high as
manifested by the large monthly extremes, especially in the summer.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/clim-CE-month-norm-MAX-median-N-MW-1} 

}

\caption{Seasonal variability of the number of CE events in Thessaloniki for the period 1993 -- 2023 normalized to the maximum occurring in June, in the form of a box and whisker plot. The monthly values have also been normalized to the relative abundance of valid GHI observations. The box contains the data between the lower $25\,\%$ and the upper $75\,\%$ percentiles, the thick horizontal line and the diamond symbol represent the median and the mean values, respectively. The vertical lines (whiskers) extend between the maximum and minimum monthly values.}\label{fig:relative-month-occurrences}
\end{figure}

The distribution of the number of CE events as a function of OI is shown in
Figure\nobreakspace{}\ref{fig:ovir-distribution}.  There is an inverse
relationship between the frequency of CE events and OI with an exponential-like
decline. This is expected, as the stronger the CE events are, the rarer the
conditions favoring the occurrence of CE events.  For the majority (over $62\,\%$) of
the CE events the OI is below the long-term average of
$40.1\,\text{W}/\text{m}^2$, while about
$8.1\,\%$ of the events correspond to OI larger than $100\,\text{W}/\text{m}^2$ and
up to the highest value of $412.4\,\text{W}/\text{m}^2$.  This distribution is
indicative of the magnitude and the probability of the expected CE events over
Thessaloniki.  Similar distribution of CE events, albeit with larger OI values, has
been reported by @Vamvakas2020, for the city of Patras. This site is located
$2.5^\circ$ south of Thessaloniki and is exposed to air masses coming mainly from the
eastern Mediterranean resulting in different cloud patterns, that may affect the
characteristics and magnitude of the CE events.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-relative-distribution-diff-2} 

}

\caption{Relative frequency distribution of CE events in Thessaloniki for the period 1993 -- 2023 as a function of OI. The histogram was split in two plots with different y-axis scales for better readability.}\label{fig:ovir-distribution}
\end{figure}

## Duration of cloud enhancement events

The duration of the CE events is variable and can last for several minutes or even
more than an hour. In order to study the characteristics of these consecutive events,
we grouped them into bins of increasing duration in steps of one minute.  We have
identified 28182 groups of CE in the whole period of study, where the group
of the longest duration of
140 minutes occurred on
07\ July 2013 in a SZA range of 
$52.1^\circ$ -- 
$77.9^\circ$.
Figure\nobreakspace{}\ref{fig:ceg-duration-distribution} shows the frequency
distribution of the CE events according to their duration.  We conclude that although
some groups of events last for more than an hour, about $80\,\%$ have duration of
less than 5 minutes.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/groups-7} 

}

\caption{Relative frequency distribution of CE groups of consequent CE events according to their duration for Thessaloniki in the period 1993 -- 2023. The histogram was split in two plots with different y-axis scales for better readability.}\label{fig:ceg-duration-distribution}
\end{figure}

The relation between the duration and the mean OI of the groups has also been
studied (Figure\nobreakspace{}\ref{fig:group-2d}).  Evidently, events of high excess
irradiation have small duration and vice versa. The vast majority of grouped events
are associated with small excess irradiation (e.g., $<5\,\text{kJ}/\text{m}^2$) and
small duration (e.g., $<5\,\text{min}$) while groups with strong excess irradiation
and long duration are very rare.  Similar results of this relation have been reported
by @Zhang2018 in a study using a far higher sampling rate ($100\,\text{Hz}$) than
ours.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-groups-bin2d-1} 

}

\caption{Relation of excess irradiation of CE groups with their duration for Thessaloniki in the period 1993 -- 2023. The logarithmic color scale denotes the frequency of the respective groups of events.}\label{fig:group-2d}
\end{figure}

<!-- ```{r echo=FALSE, fig.cap="Relation of mean OI and CE group duration"} -->
<!-- knitr::include_graphics("../images/groups-4.png") -->
<!-- ``` -->

<!-- ```{r echo=FALSE, fig.cap="Relation of maximun OI and CE group duration"} -->
<!-- knitr::include_graphics("../images/groups-8.png") -->
<!-- ``` -->


## Extreme cloud enhancement events

An aspect of the CE events that is commonly reported and has some significance on the
solar energy production infrastructure are the extreme CE events (ECE), where solar
irradiance exceeds the expected irradiance on top of the atmosphere at the same SZA
(Equation\nobreakspace{}\ref{eq:ECE}).  Analogous to
Figure\nobreakspace{}\ref{fig:relative-month-occurrences} we have computed the
distribution of the number of occurrences of ECE events by month in
Figure\nobreakspace{}\ref{fig:relative-month-occurancies-ECE}. The most active period
for ECE events is in spring and early summer (March -- June), followed by a
period in early autumn (September and October).  This is probably related to the
weather characteristics in these periods, with frequent alternations between clear
sky periods and clouds.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/clim-ECE-month-norm-MAX-median-N-2} 

}

\caption{Seasonal variability of the number of ECE events in Thessaloniki for the period 1993 -- 2023 normalized to the maximum occurring in March, in the form of a box and whisker plot. The box contains the data between the lower $25\,\%$ and the upper $75\,\%$ percentiles. The thick horizontal line and the diamond symbol represent the median and the mean values, respectively. The vertical lines (whiskers) extend between the maximum and minimum monthly values.}\label{fig:relative-month-occurancies-ECE}
\end{figure}

<!-- ```{r echo=FALSE, fig.cap="Distribution of the number of ECE cases"} -->
<!-- knitr::include_graphics("../images/climECEmonth-1.png") -->
<!-- ``` -->

The distribution of the ECE events
(Figure\nobreakspace{}\ref{fig:P-extreme-distribution}) shows that in rare cases the
OI can exceed the TSI even by more than $400\,\text{W}/\text{m}^2$, while in
$75\,\%$ of the cases the OI is below $200\,\text{W}/\text{m}^2$. The OI for the
most frequent ECEs ranges between $140$ and $180\,\text{W}/\text{m}^2$.  These
findings are in accordance with the results of @Vamvakas2020, the only difference
being that the OI values reported for are higher than those for Thessaloniki.


\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-extreme-distribution-2} 

}

\caption{Distribution of ECE events in Thessaloniki for the period 1993 -- 2023. Measured as extra irradiance over TSI at TOA for the same SZA.}\label{fig:P-extreme-distribution}
\end{figure}


<!-- ```{r P-extreme-distribution, echo=FALSE, fig.cap="Distribution of ECE events in Thessaloniki for the period 1993 -- 2023."} -->
<!-- knitr::include_graphics("../images/P-extreme-distribution-1.png") -->
<!-- ``` -->



<!-- ```{r echo=FALSE, fig.cap="Distribution of ECE Irradiance above 'TSI'"} -->
<!-- knitr::include_graphics("../images/extremedistributions-4.png") -->
<!-- ``` -->

<!--  SZA -->

<!-- ### Yearly plots relative to all GHI data. -->
<!-- ```{r echo=FALSE, fig.cap=""} -->
<!-- knitr::include_graphics("../images/rel_energy-1.png") -->
<!-- ``` -->

<!-- ```{r echo=FALSE, fig.cap=""} -->
<!-- knitr::include_graphics("../images/rel_energy-2.png") -->
<!-- ``` -->

<!-- ```{r echo=FALSE, fig.cap=""} -->
<!-- knitr::include_graphics("../images/rel_energy-3.png") -->
<!-- ``` -->


# Conclusions

By creating a clear sky approximation representing the long- and short-term
variability of the expected clear sky GHI, we were able to identify cases of CE
events in Thessaloniki for the period 1993 -- 2023.  After analyzing the CE cases, we
found an increase of
$+45.6\pm 22\,\text{cases}/\text{year}$,
with the mean annual irradiation of the CE events increasing with a rate of
$+116.3\pm 68.2\,\text{kJ}/\text{year}$.
The most active months of CE events are May and June.  We found that continuous CE
events can last up to $140$ minutes, while the duration of $80\,\%$ of them is bellow
$5$ minutes.

We have observed ECE cases exceeding the TSI by $400\,\text{W}/\text{m}^{2}$ with
$75\,\%$ of the cases under $200\,\text{W}/\text{m}^{2}$. The climatological
characteristics of the ECE events showed that the most active months are spread in
half of the year and particularly in the periods March -- June and September --
October.  The magnitude of the ECE events identified in Thessaloniki events does not
exceed the values reported for sites with more favourable conditions for the
phenomenon [e.g., @Cordero2023]. Some of the characteristics of CE and ECE events we
analyzed have strong similarities with the results of @Vamvakas2020 for the city of
Patras, south of Thessaloniki, albeit with differences in the magnitude of OI.

An interpretation of the CE trends shows that the interaction of GHI with the clouds,
through this 30-year period, is a dynamic phenomenon that needs further
investigation. For such studies it would be essential to have more detailed
information on cloud characteristics, especially in order to investigate possible
associations of the observed trends with changes in climate.



<!-- END -->
