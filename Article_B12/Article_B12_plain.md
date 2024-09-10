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
# abstract: |
#   In this study we investigate the characteristics of global horizontal irradiance
#   \(200\,\text{W}/\text{m}^{2}\). The most active period of these extreme events is
#   spring -- early summer with a secondary peak in autumn.

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
date:           "2024-09-09"
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

In this study, we investigate the characteristics of global horizontal irradiance
enhancement events induced by clouds over Thessaloniki for the period 1994 -- 2023
using data recorded every one minute.  To our knowledge, there isn't a similar study
with such a long time series.  We identified the cloud enhancement (CE) events by
creating an appropriate cloud-free irradiance reference using a radiative transfer
model and aerosol optical depth data from a collocated Cimel sun photometer and a
Brewer spectrophotometer. We found a trend in CE events of
$+112\pm
35\,\text{cases}/\text{year}$,
and a trend in the corresponding irradiation of
$+329.9\pm
112\,\text{kJ}/\text{year}$.
The peak of the CE events was observed during May and June. CE events with duration
longer than 10\nobreakspace{}min are very rare
($<8\,\%$), with exceptions lasting
over an hour and up to 140 minutes.  Finally, we have detected enhancements above the
total solar irradiance at the top of the atmosphere for the same solar zenith angle
of up to
$204\,\text{W}/\text{m}^{2}$, with the $75\,\%$ of the cases below
$40\,\text{W}/\text{m}^{2}$. Most of these extreme events occur in spring -- early
summer, with a secondary peak in autumn.



# Introduction

The shortwave solar radiation, reaching Earth's surface, is the main energy source
for the atmosphere and the biosphere, and drives and governs the climate [@Gray2010].
It has direct practical application in industries related to energy and agricultural
production. The variability of its intensity can impose difficulties in predicting
the yield and in designing the specifications of the appropriate equipment.  A
significant portion of the relevant research has been focused on predicting the
renewable energy production in a fine timescale and in near real-time [for a review
see @Inman2013; @Graabak2016].

An important aspect of the variability of solar radiation is its interaction with the
clouds. In general, clouds can attenuate a fraction of solar irradiance, but under
certain conditions, can lead to enhancement of the global horizontal irradiance (GHI)
reaching the ground. This cloud enhancement (CE) effect can locally increase the
observed GHI to levels even higher than the expected cloud-free irradiance
[@Cordero2023; @Vamvakas2020; @CastillejoCuberos2020;  and references
therein].

Some of the proposed underling mechanisms of those enhancements have been summarized
by @Gueymard2017; the most important being the scattering of radiation on the edges
of cumulus clouds.  It has also been suggested that enhancement of GHI can be
produced by thin cirrus clouds though refraction and scattering [@Thuillier2013].
Further investigation with radiative transfer modeling and observations pointed as
the prevailing mechanism, the strong forward Mie scattering through clouds of low
optical depth [@Pecenak2016; @Thuillier2013; @Yordanov2013; @Yordanov2015].  Overall,
the appearance of CE events depend on different interactive factors, which include
cloud thickness, structure and type, and the relative position of the sun and the
clouds [@Gueymard2017; @Veerman2022].

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
there is no evidence of whether this trend has affected also the number and strength
of CE events.

<!--
Study of the mechanism has also been done with modeling of different cloud types and
atmospheric conditions.  @Gueymard2017; @Veerman2022

Instruments response ....
@Martins2022
-->

Methods of identification of CE events usually include the use of simulated
cloud-free irradiance as baseline, combined with an appropriate threshold or some
other statistical characteristics, and in some cases, with visual inspection of sky
camera images [@Vamvakas2020; @Mol2023 and references therein].

<!-- our work -->
In this study, we evaluate the effects of CE events on GHI by investigating their
frequency of occurrence, intensity, and duration in a thirty-year record of GHI
observations at Thessaloniki, Greece.  We used modeled cloud-free irradiance as a
baseline to identify cloud enhancements, and we determined long-term trends of the
above-mentioned metrics, their climatology and some general characteristics.  To our
knowledge, there are no other studies that provide trends from such a long dataset.



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
calibrations.  We note that the specific model of the pyranometer is not capable in
recording the instrument's temperature, therefore any temperature changes that may
occur during the day (including those due to sudden increases or decreases in
incidence irradiance) were not considered in the data reduction procedure. However,
the sensitivity of the instrument to temperature is less than $\pm1\,\%$ for its
operational range ($-20^\circ$ -- $50^\circ$C).

<!-- Radiation data quality check -->
To further improve the quality of the irradiance data, a manual screening was
performed, to remove inconsistent and erroneous recordings that can occur
stochastically or systematically during the long operation of the instrument.  The
manual screening was aided by a radiation data quality assurance procedure, adjusted
for the site, which was based on the methods of Long and Shi\ [@Long2006;
@Long2008a].  Thus, problematic recordings have been excluded from further
processing.  Furthermore, due to the significant measurement uncertainty in GHI when
the Sun is near the horizon, and due to some systematic obstructions by nearby
buildings, we have excluded all measurements with solar zenith angle (SZA) greater
than $78^\circ$.  Finally, images from a sky camera have been used in
the manual inspection of the CE identification. The sky camera has been operating
since 2012 and stores images in 5 min time steps.  An overview of the GHI data used
in this study is given in
Figure\nobreakspace{}\ref{fig:CLB-daily}, as the daily means.  Daily means were
calculated only for days with at least $60\,\%$ of data availability; however, all
available one-minute measurements have been used for the detection of CE events.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P_daily_trend-5} 

}

\caption{Timeseries of daily mean GHI measured at Thessaloniki for the period 1994 -- 2023.}\label{fig:CLB-daily}
\end{figure}



## Cloud enhancement detection

<!-- Define CE and ECE tomove? -->
In this study, we define an event as CE when the measured GHI at ground level
exceeds the expected value under cloud-free conditions. Similarly, we define as
extreme cloud enhancement events (ECE), the cases when GHI at ground level exceeds
the Total Solar Irradiance (TSI) at the top of the atmosphere (TOA) for the same SZA.
Although the duration of these bursts can vary from seconds to several minutes, here
we are constrained by the temporal resolution of our data to identify events with a
duration of at minimum one-minute.

For the detection of CE cases, we established a baseline of irradiance above which we
characterized each data point as an enhancement event and calculated the over
irradiance (OI). The OI is defined as the irradiance difference of the measured
one-minute GHI from the $\text{GHI}_\text{ref}$ corresponding to cloud-free
atmosphere.  First, we used a simple approach for the determination of
$\text{GHI}_\text{ref}$: The Haurwitz's model [@Haurwitz1945], which is a simple
clear sky radiation model and was already adjusted and applied to our data
[@Natsis2023].  We created a threshold by using an appropriate relative and/or an
additional constant offset.  The initial results showed that we can detect a big
portion of the actual CE events.  However, by inspecting the daily plots of
irradiance, it became evident that changes in atmospheric conditions introduced
numerous false positive or false negative results. The main reason for these
discrepancies is the variability of the effects of aerosols and water vapor which
were not considered in the simple method.  To produce a more representative reference
we included the effects of these factors using a radiative transfer model (RTM).  The
applied methodology is discussed in section\nobreakspace{}\ref{rtmcs}.

## Modeled cloud-free Irradiance {#rtmcs}

### Climatology of cloud-free irradiance

<!-- Libradtran options -->
We approximated the expected cloud-free $\text{GHI}_\text{ref}$ with the radiative
transfer model uvspec, part of libRadtran [@Emde2016], similarly to the approach used
by @Vamvakas2020.  In uvspec we used the solar spectrum of @Kurucz1994 in the range
$280$ to $2500\,\text{nm}$, the radiative transfer solver "disort" in
"pseudospherical" geometry and the "LOWTRAN" gas parameterization.  The model was run
for climatological values of the Angstrom coefficients, water column (WC), SZA and
the appropriate seasonal atmospheric profile to create a look-up table (LUT) for the
estimation of the cloud-free reference irradiance for each individual observation of
our dataset. In this context, the model was run for SZAs in the range $10$ --
$90^\circ$ with a step of $0.2^\circ$ and for the atmospheric profiles of the Air
Force Geophysics Laboratory [@Anderson1986] midlatitude summer and midlatitude
winter, representative of the warm and cold seasons.


Main factors responsible for the attenuation of the broadband downward solar
radiation under cloud free atmospheres are aerosols and water vapor.  At
Thessaloniki, such measurements are available since 2003 from a Cimel sun photometer,
which is part of the Aerosol Robotic Network (AERONET) [@Giles2019; @Buis1998].  From
the observations in the period 2003 -- 2023 we calculated the monthly climatological
means and standard deviation ($\sigma$) for the aerosol optical depth (AOD) at
$500\,\text{nm}$ and the equivalent height of the water column (WC).  The monthly
climatological values of AOD and WC, as well as combinations with additional offsets
of $\pm1\sigma$ and $\pm2\sigma$, were used as inputs to the RTM in the construction
of the LUT.

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
The variable parameters of LUT consists of the month along with the relevant seasonal
atmospheric profile, the climatological values of the alpha, beta parameters of the
Ångström equation for 500nm derived from the 340nm, the WC, with the perturbation as
described above, and the SZA of the simulation.

For the application of  the LUT, to account for the Sun's variability, in our
one-minute GHI measurements, we adapted each modeled value by scaling the model's
input spectrum integral, to the corresponding TSI, provided by NOAA
[@Coddington2005].  Also, we account for the effect of the Earth -- Sun distance on
the irradiance, by using the distance calculated by the Astropy
[@AstropyCollaboration2022] software library.  As needed, we interpolate the
resulting irradiances to the SZA of our measurements.  For each period of the year,
we used the appropriate atmospheric profile (afglms or afglmw).  Finally, we
calculated the cloud-free irradiance value at the horizontal plane.  With this
method, the modeled cloud-free irradiances schemes can be directly compared to each
measured one-minute value of GHI.
-->


### Long-term change of cloud-free irradiance

The cloud-free reference values discussed above are based on the climatological AOD
and WC; hence they cannot describe accurately the long-term variation of
$\text{GHI}_\text{ref}$ due to long-term changes in the two atmospheric constituents,
mainly AOD.  As reported by @Natsis2023, there is a long-term brightening effect in
the GHI data of Thessaloniki for the period 1993 -- 2023, which for cloud-free data
was attributed to long-term changes in aerosol effects.  Therefore, an adjustment of
the $\text{GHI}_\text{ref}$ during the period of study was made using RTM simulations
to account for the long-term variations of the AOD.  High quality AOD data with a
Cimel sun-photometer (part of AERONET) at Thessaloniki start only in 2003, while
spectral AOD measurements using direct-sun observations with a MKIII Brewer
spectrophotometer are available for the period 1997 -- 2017. These data are taken
sporadically during each day and are less dense compared to the AERONET data. By
comparing monthly AOD data at $340\,\text{nm}$ of the two instruments for the common
periods of operation we adjusted the Brewer data and filled the missing months of the
Cimel record data with Brewer data.  Using the monthly time series of AOD at
$340\,\text{nm}$, as well as monthly climatological values of the Ångström exponent
and constant WC of $15.6\,\text{mm}$ derived from the Cimel record, we simulated with
the RTM the cloud-free GHI at SZA of $55^\circ$ for each month in the period 1997 --
2023. The SZA of $55^\circ$ was chosen as representative of all days in the year to
get a rough estimate of the annually averaged change in cloud-free GHI. A
second-degree polynomial fit was applied to the simulated yearly averaged GHI
to derive the long-term change in GHI due to aerosols:
\begin{equation}
\Delta(\text{AOD}) [\%] = -12170 + 12.05 \cdot y + -0.002981 \cdot y^2 \label{eq:AODchange}
\end{equation}
where $y$ is the date as a decimal fraction of the year. For the period of the study
the relative change of AOD ranges between $-1.88$ and $-0.23\,\%$.  Finally, we
applied these long-term changes of Equation\nobreakspace{}\ref{eq:AODchange} to the
climatological $\text{GHI}_\text{ref}$, to create a more realistic representation of
the cloud-free irradiance for the entire period of study. For the period 1994 -- 1996
where no AOD data are available, we assumed that the changes in GHI follow the same
polynomial fit.

<!--
As AERONET data
start only in 2003, we used for the period 1994 -- 2005 estimates of changes in AOD
at $340\,\text{nm}$ derived from a collocated Brewer spectrophotometer
[@Kazadzis2007] to calculate the trend in $\text{GHI}_\text{ref}$ due to aerosols
during this period.


To create a unified trend for the long-term change of the cloud-free irradiance, we
simulated the values of AOD derived from those sources with Libradtran. For both
inputs, we used the AOD at $500\,\text{nm}$, which was inferred by the available
Ångström coefficients. We choose the SZA of $55^\circ$ as a representative value for
all the runs.
-->

<!--
According to @Kazadzis2007, in the period 1997 -- 2005 the mean AOD at $340\,\text{nm}$
is $0.4$ with a change of $-3.8\pm0.93\,\%$ per year, corresponding to a change of
$0.0153$ per year.  Using an Ångström coefficient $\alpha = 1.6$, this translates to
a change in the Ångström coefficient $\beta$ of $0.00272$ per year (or $\beta=0.084$ in
1997 and $\beta=0.059$ in 2005).
Simulations with uvspec for the above Ångström coefficients, assuming constant WC of
$15.6\,\text{mm}$ taken from the Cimel, and for a SZA of $55^\circ$ reveal a change of
$+0.21\,\%$ per year in $\text{GHI}_\text{ref}$.  The SZA of $55^\circ$ was chosen as
representative of all days in the year in order
to get a rough estimate of the annually averaged change in cloud-free irradiance.  For
the period 2005 -- 2023 we used the mean monthly values of AOD and WC from AERONET in
a similar simulation scheme to calculate the monthly mean cloud-free irradiance,
and finally the change of $+0.14\,\%$ per year. We applied these two long-term changes
(see Figure\nobreakspace{}\ref{fig:CS-change}) to the climatological
$\text{GHI}_\text{ref}$, to create a more realistic representation of the cloud-free
irradiance for the whole period of study.  The rate of changes in the two subperiods
are different, but reasonable, since the effect of the enforced air pollution
abatement measures in the area in the 1990s had greater effects in the first years of
application.
-->

<!--
\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-CS-change-1} 

}

\caption{Simulated long-term change in cloud-free irradiance relative to the climatological values due to changes in AOD in Thessaloniki for the period 1994 -- 2023.}\label{fig:CS-change}
\end{figure}
-->

<!--
GHI_model (%)
 = -12170 + 12.05 * year_frac + -0.002981 * year_frac^2
-->


## Criteria for the identification of CE events

One of the goals of this study was to quantify the OI related to CEs. A key issue for
achieving this goal is to define a threshold for the CE identification,
representative of the cloud-free irradiance at the time of each GHI measurement. This
depends on the selection of the appropriate atmospheric parameterization for the RTM
simulations. The implementation of the long-term change in AOD, discussed in
section\nobreakspace{}\ref{rtmcs}, allows capturing a large part of the natural
variability of cloud-free GHI. However, the short-term variability of AOD cannot be
taken adequately into account when using monthly values in the model simulations. We
tried different approaches to strengthen the robustness of the methodology and to
compensate for the limited accuracy of the RTM input data and the unpredictable
natural variability of the atmosphere.

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
excluding clouds.  With this approach, the simulated $\text{GHI}_\text{ref}$ should be
generally greater than the measured GHI when aerosols are more abundant. The
correlation of the $\text{GHI}_\text{ref}$ with the GHI was tested for a subset of
cloud-free days with more than $80\,\%$ data availability
(Figure\nobreakspace{}\ref{fig:validation-GHI}) and found to have a positive bias of
more than $18\,\text{W}/\text{m}^2$ across all levels of radiation.  To achieve a
robust and clear distinction of CE cases, we set the criteria of CE detection
$25\,\text{W}/\text{m}^2$ above the $\text{GHI}_\text{ref}$.

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../images/P-validation-cloudfree-GHI-1} 

}

\caption{The corelation GHI with $\text{GHI}_\text{ref}$, for cloud-free data of days with data availability more than $80\,\%$.}\label{fig:validation-GHI}
\end{figure}

<!--
To
compensate for this, we defined the following threshold $E$ to compare the
measured $\text{GHI}$:
\begin{equation}
\text{CE} : E > 25 + 1 \cdot \text{GHI}_\text{ref} \,\,[\text{W}/\text{m}^2] \label{eq:CE4}
\end{equation}
-->
<!--
This is the criterion of our CE identification.  The constant terms were determined
through the implementation of an empirical method
-->

To further ensure the effectiveness of this CE criterion, we implement a manual
inspection of the CE identification results on selected days of the whole dataset.
We tested seven sets of days with different characteristics relevant to the
efficiency of the identification threshold.  These sets were random groups of about
20 -- 30 days with the following characteristics:
(a) the strongest OI CE events,
(b) the largest daily total OI,
(c) absence of clouds (by implementing a cloud identification algorithm as discussed in @Natsis2023),
(d) absence of clouds and absence of CE events,
(e) with at least $60\,\%$ of the day length without clouds and presence of CE events,
(h) randomly selected days, and
(i) manually selected days.
For the latter case and where needed, we also used images from the sky-camera to
further aid the decision of the manual inspection.

<!-- reasoning for criteria -->
<!-- We have confirmed, by inspecting images from the sky cam, that we have
<!-- duration of elevated irradiance, either due to the clearness of the atmosphere, or some
<!-- interferences by reflections on nearby bright surfaces. Although, these cases may
<!-- mask some of the CE events. The actual CE events, produce higher irradiance and thus
<!-- are identified as such. As a side effect, the reported OI will be slight
<!-- underestimated for those SZAs, but the overall contribution of those cases to the
<!-- total daily energy, is minimal due the low occurrences and the lower irradiances.
-->

The definition of the CE events with this method has a degree of subjectivity, since
the actual cloud-free irradiance is not known and can only be approximated.  However,
this method was proven capable in detecting all major CE events.  Where some CE
events with very low OI may be not detected, these are few with small
over-irradiance, and it is unlikely that will affect significantly our results.

<!-- extreme caces criterion -->
A sub-category of the CE events that is often discussed in the literature
[@Cordero2023; @Martins2022; @Yordanov2015], are the extreme cloud enhancement (ECE)
events. These are cases of CE where the measured intensity of the irradiance exceeds
the TSI on horizontal plane at TOA. In this case the threshold $E$ is given by:
\begin{equation}
\text{ECE}: E > \cos(\text{SZA}) \cdot E_{\odot} \frac{r^2_\text{m}} {r^2}
\label{eq:ECE}
\end{equation}
where: $\text{SZA}$ the solar zenith angle, $E_{\odot}$ is the solar constant at the
mean Sun -- Earth distance ($1367\,\text{W}/\text{m}^2$), $r$ is
the actual Sun - Earth distance and $r_\text{m}$ is the mean Sun -- Earth distance of
$1.496\times10^8\,\text{km}$.

An example of the identification of CE events for a selected day is given in the
Figure\nobreakspace{}\ref{fig:example-day}, where the daily course of the cloud-free
reference irradiance and the CE and ECE thresholds are shown along with the actual
GHI measurements.  The GHI data in the periods 7:30 -- 8:30 and after 14:00 are very
close to the modeled cloud-free GHI (light blue line) thus were identified as
cloud-free instances. There is some ambiguity for the data points laying between the
CE threshold and the cloud-free irradiance, which are not identified as CE events.
These data points correspond to cases either with AOD below the one assumed in the
model, or with very small OI.

In addition, we provide in Figure\nobreakspace{}\ref{fig:example-year} an example
scatter plot between the measured GHI and the modeled cloud-free irradiance for one
year (2005), where the CE and ECE events are clearly grouped above the cloud-free
irradiance (approximated with the green line).  The grey colored data points above
this line correspond to the ambiguous data points discussed above. Finally, the black
belt just below the green line is formed from data measured mainly under cloud-free
conditions,conditions with the spread likely caused by the short-term variability of
AOD. A small fraction of these data corresponds also to cases with thin cirrus clouds
causing weak attenuation of GHI, often indistinguishable from the attenuation by
aerosols.


\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../images/P-example-day-5} 

}

\caption{Example of CE identification in Thessaloniki for 2019-07-11. The green line with blue symbols depicts the measured GHI in one-minute steps. The red line shows the modelled threshold for the detection of CE events, which are denoted with red circles. The black line represents the TOA TSI on a horizontal plane, equivalent to the threshold for the identification of ECE events, shown with red circles. The purple line is the modeled cloud-free irradiance. The dark yellow line is the solar constant of $1367\,\text{W}/\text{m}^{2}$.}\label{fig:example-day}
\end{figure}

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../images/P-example-years-12} 

}

\caption{Example scatter plot of the measured GHI and the reference cloud-free irradiance in Thessaloniki for the year 2005. The over-irradiance for CE and ECE events is colour coded, while the remaining data points are shown in black. The reference green line goes through the origin with a slope of one.}\label{fig:example-year}
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
$2.32\,\%$ were identified as CE events and
$0.036\,\%$ as ECE events.
The highest recorded GHI due to CE was
$1416.6\,\text{W}/\text{m}^2$ on
 24\ May 2007 at a SZA of
$19.9^\circ$ corresponding to OI of
$369.3\,\text{W}/\text{m}^2$ or
$35.3\,\%$ above the threshold.
The strongest CE event of
$53\,\%$ above the 
cloud-free threshold was observed on 
28\ October 2016 at a SZA of
$59.2^\circ$ with a GHI value of 
$861.8\,\text{W}/\text{m}^2$ and a OI of
$298.4\,\text{W}/\text{m}^2$.
Both cases are also ECE events with
$131.2\,\text{W}/\text{m}^2$ and
$161.5\,\text{W}/\text{m}^2$ above the
TSI at TOA for the same SZA, respectively.
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
CE cases (each with duration of one minute), the yearly mean OI and the yearly excess
irradiation for the period 1994 -- 2023, together with corresponding linear trends.
To account for missing data, all three quantities have been divided with the fraction
of the valid GHI observations in each month.  Statistically significant (at the
$95\,\%$ confidence level) increasing trends appear for the yearly number of CE
events
($+112\pm 35\,\text{cases}/\text{year}$)
and the excess irradiation
($+329.9 \pm 112\,\text{kJ}/\text{year}$),
while the the trend in the yearly mean OI is negligible
($+0.11\pm 0.2\,\text{W}/\text{m}^2/\text{year}$)
and of no statistical significance.
The average OI for the entire period is
$+42.6\pm 2.8\,\text{W}/\text{m}^2$.
The yearly excess irradiation due to CE events ranges between about $8$ and
$24\,\text{MJ}/\text{m}^2$. On average, it is about one half of the highest daily
irradiation of \~31 MJ/m^2^ under cloud-free conditions recorded at the summer
solstice in Thessaloniki.  Therefore, from solar energy perspective the yearly excess
irradiation is only a tiny fraction of the available solar energy. Although the
interannual variability of the mean OI is rather weak, the variability about the
trend lines is quite large for the number of CE cases and the excess irradiation
(panels b and c). For these two quantities the spread tends to increase with time,
suggesting a significant variability in cloud patterns over the area, possibly
associated to changes in climate.

We must note that the excess irradiation from the CE events that is received at the
surface does not affect the balance of the energy budget in the atmosphere. The
incoming solar radiation in the region is not increased by the OI but is rather
redistributed through the CE events.  This is also depicted by the ECE irradiance
values, which exceed the equivalent cloud-free irradiance by a significant amount.

\begin{figure}% [h!]
        {\centering
            \subfloat[\label{fig:P-energy-N}]
                {\includegraphics[width=\linewidth]{../images/P-energy-complete-2} }\\
            \subfloat[\label{fig:P-energy-mean}]
                {\includegraphics[width=\linewidth]{../images/P-energy-complete-3} }\\
            \subfloat[\label{fig:P-energy-sum}]
                {\includegraphics[width=\linewidth]{../images/P-energy-complete-1} }
        }
    \caption{Time series of (a) the yearly CE number of occurrences, (b) the yearly mean OI and (c) the yearly excess irradiation at Thessaloniki for the period 1994 – 2023. The black lines represent the linear trends on the yearly data.}\label{fig:P-energy}
\end{figure}


## Climatology of cloud enhancement events

Next, we investigated the distribution of the CE events within the year.
Figure\nobreakspace{}\ref{fig:relative-month-occurrences} shows the monthly box and
whisker plot of the number of occurrences of the CE events in percent, calculated
relative to the available one-minute data in each month.  Although CE events are
present throughout the year, showing a clear seasonal cycle, the most active months
are May and June, with their median representing about $3\,\%$ of the data.  The
least frequent events (about $1\,\%$) occur in August and during winter (December --
February).  This seasonality is a combined effect of different factors, among them
the types of clouds, their frequency of occurrence, the seasonally varying relative
position of the sun, as well as the local landscape characteristics that may
influence the formation of the clouds.  For example, during May and June the frequent
formation of cumulus clouds in the area leads to a significant increase in the CE
events.  However, the lack of detailed data in the area of cloud characteristics does
not allow further analysis. Finally, the interannual variability of the monthly CE
events is quite high as manifested by the size of the boxes and the large monthly
extremes, especially in the summer. Investigation of the causes of this variability
is beyond the scope of this study. It could possibly be related to the cloud cover
variability in winter and autumn over southern Europe, associated with the North
Atlantic Oscillation (NAO) circulation [@Chiacchio2010], or to the observed
decreasing trend in cloud cover as a result of global warming [e.g.,
@SanchezLorenzo2017].

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-CE-climatology-normlz-1} 

}

\caption{Seasonal variability of the relative occurrence of CE events in Thessaloniki for the period 1994 -- 2023, in the form of a box and whisker plot. The box contains the data between the lower $25\,\%$ and the upper $75\,\%$ percentiles, with the thick horizontal line representing the median. The vertical lines (whiskers) extend between the maximum and minimum monthly values.}\label{fig:relative-month-occurrences}
\end{figure}

<!--
\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/clim-CE-month-norm-MAX-median-N-MW-1} 

}

\caption{Seasonal variability of the number of CE events in Thessaloniki for the period 1994 -- 2023 normalized to the maximum occurring in June, in the form of a box and whisker plot. The monthly values have also been normalized to the relative abundance of valid GHI observations. The box contains the data between the lower $25\,\%$ and the upper $75\,\%$ percentiles, the thick horizontal line and the diamond symbol represent the median and the mean values, respectively. The vertical lines (whiskers) extend between the maximum and minimum monthly values.}\label{fig:relative-month-occurrence}
\end{figure}
-->


The distribution of the number of CE events as a function of OI is shown in
Figure\nobreakspace{}\ref{fig:ovir-distribution}.  There is an inverse relationship
between the frequency of CE events and OI with an exponential-like decline. This is
expected, as the stronger the CE events are, the rarer the conditions favouring the
occurrence of CE events.
For the majority (over $62\,\%$) of
the CE events the OI is below the long-term average of
$42.6\,\text{W}/\text{m}^2$, while
$9.6\,\%$ of the events correspond to OI larger than
$100\,\text{W}/\text{m}^2$ and up to the highest value of
$437\,\text{W}/\text{m}^2$.
This distribution is indicative of the magnitude and the probability of the expected
CE events over Thessaloniki.  Similar distribution of CE events, albeit with larger
OI values, has been reported by @Vamvakas2020, for the city of Patras. This site is
located $2.5^\circ$ south of Thessaloniki and is exposed to air masses coming mainly
from the eastern Mediterranean, resulting in different cloud patterns, that may
affect the characteristics and magnitude of the CE events.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-relative-distribution-diff-2} 

}

\caption{Relative frequency distribution of CE events in Thessaloniki for the period 1994 -- 2023 as a function of OI. The histogram was split in two plots with different y-axis scales for better readability.}\label{fig:ovir-distribution}
\end{figure}

## Duration of cloud enhancement events

The duration of the CE events is variable and can last for several minutes or even
more than an hour. To study the characteristics of these consecutive events, we
grouped them into bins of increasing duration in steps of one minute.  We have
identified 34849 groups of CE events in the whole period of study, where
the group with the longest duration of
140 minutes occurred on
07\ July 2013
with SZA ranging between
$52.1^\circ$ and
$77.9^\circ$.
Figure\nobreakspace{}\ref{fig:ceg-duration-distribution} shows the frequency
distribution of the CE events according to their duration.  We conclude that although
some groups of events last for more than an hour, about
$79\,\%$ have a duration of
equal or less than 5 minutes.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/groups-7} 

}

\caption{Relative frequency distribution of CE groups of consequent CE events according to their duration for Thessaloniki in the period 1994 -- 2023. The histogram was split in two plots with different y-axis scales for better readability.}\label{fig:ceg-duration-distribution}
\end{figure}

The relation between the duration and the mean OI of the CE groups is shown in
Figure\nobreakspace{}\ref{fig:group-2d}.  As expected, events with small duration and
low OI are accumulated close to the origin of the plot.  Events of high excess
irradiation have a short duration and vice versa. The vast majority of grouped events
are associated with small excess irradiation ($<5\,\text{kJ}/\text{m}^2$) and short
duration ($<5\,\text{min}$). For example, the excess irradiation of
$5\,\text{kJ}/\text{m}^2$ for $5\,\text{min}$ is equivalent to an OI of
$16.7\,\text{W}/\text{m}^2$ with duration of $5\,\text{min}$. On the contrary, groups
with strong excess irradiation and long duration are very rare.  Similar results of
this relation have been reported by @Zhang2018 in a study using a far higher sampling
rate ($100\,\text{Hz}$) than ours.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-groups-bin2d-1} 

}

\caption{Relation of excess irradiation of CE groups with their duration for Thessaloniki in the period 1994 -- 2023. The logarithmic color scale denotes the frequency of the respective groups.}\label{fig:group-2d}
\end{figure}


## Extreme cloud enhancement events

An aspect of the CE events that is commonly reported and has some significance on the
solar energy production infrastructure are the extreme CE (ECE) events, where the
measured GHI exceeds the expected irradiance at the TOA at the same SZA
(Equation\nobreakspace{}\ref{eq:ECE}).  In analogy to
Figure\nobreakspace{}\ref{fig:relative-month-occurrences}, we show in 
Figure\nobreakspace{}\ref{fig:relative-month-occurancies-ECE} the number of ECE
events relative to the total number of one-min observations in each month. Judging by
the distribution of the monthly medians, the most active months in ECE events are
January -- March and November and the least active are July and August. The highest
interannual variability in the 30-year record occurs from October to April. In the
summer period, the fraction of ECE events is very small, with almost zero variability
in July and August.  Although the most active months for CE events are May and June
(Figure\nobreakspace{}\ref{fig:relative-month-occurrences}), these months behave
differently for the ECE events, suggesting that the cumulus clouds that usually cause
enhancements of irradiance are incapable to produce extreme events, possibly because
of their size and structure, in conjunction with the smaller SZAs in the summer.

In general, the fraction of ECE events during the year is very small, well below
$0.1\,\%$ of the data, therefore on average it would not affect the production of
solar energy. However, during short, isolated periods extreme enhancements of GHI can
be of concern.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-ECE-climatology-normlz-1} 

}

\caption{Seasonal variability of the relative occurrence of ECE events in Thessaloniki for the period 1994 -- 2023, in the form of a box and whisker plot. The box contains the data between the lower $25\,\%$ and the upper $75\,\%$ percentiles, with the thick horizontal line representing the median. The vertical lines (whiskers) extend between the maximum and minimum monthly values.}\label{fig:relative-month-occurancies-ECE}
\end{figure}
<!--
\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/clim-ECE-month-norm-MAX-median-N-2} 

}

\caption{Seasonal variability of the number of ECE events in Thessaloniki for the period 1994 -- 2023 normalized to the maximum occurring in March, in the form of a box and whisker plot. The box contains the data between the lower $25\,\%$ and the upper $75\,\%$ percentiles. The thick horizontal line and the diamond symbol represent the median and the mean values, respectively. The vertical lines (whiskers) extend between the maximum and minimum monthly values.}\label{fig:relative-month-occurancies-ECEs}
\end{figure}
-->

The distribution of the ECE events
(Figure\nobreakspace{}\ref{fig:P-extreme-distribution}) shows that in rare cases the
measured GHI exceeds the TOA TSI at the same SZA even by more than
$200\,\text{W}/\text{m}^2$, while in $75\,\%$ of the cases the OI is below
$40\,\text{W}/\text{m}^2$. The $50\,\%$ OI of the most frequent ECEs cases ranges
between $127$ and $211\,\text{W}/\text{m}^2$.  These findings are in accordance with
the results of @Vamvakas2020, the only difference being that the OI values reported
are higher than those for Thessaloniki.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{../images/P-extreme-distribution-2} 

}

\caption{Relative frequency distribution of ECE events, in Thessaloniki for the period 1994 -- 2023, as a function of the extra irradiance over TSI at TOA for the same SZA.}\label{fig:P-extreme-distribution}
\end{figure}


# Conclusions

By creating a cloud-free approximation representing the long- and short-term
variability of the expected cloud-free GHI, we were able to identify cases of CE
events in Thessaloniki for the period 1994 -- 2023.  After analyzing the CE cases, we
found an increase of
$+112.1\pm 34.9\,\text{cases}/\text{year}$,
with the mean annual irradiation of the CE events increasing with a rate of
$+329.9\pm 112\,\text{kJ}/\text{year}$.
The most active months of CE events are May and June.  We found that continuous CE
events can last up to $140$ minutes, while the duration of $79\,\%$ of them is equal
or bellow $5$ minutes.

We have observed ECE cases with GHI exceeding the TOA TSI on horizontal plane by more
than $200\,\text{W}/\text{m}^{2}$ while for $75\,\%$ of the cases the excess
irradiance (relative to TOA TSI on horizontal plane) is below
$40\,\text{W}/\text{m}^{2}$. The climatological characteristics of the ECE events
showed that the most active months are spread over half of the year, and particularly in
the periods March -- June and September -- October.  The magnitude of the ECE events
identified in Thessaloniki events does not exceed the values reported for sites with
more favourable conditions for the phenomenon [e.g., @Cordero2023]. Some of the
characteristics of CE and ECE events we analyzed have strong similarities with the
results of @Vamvakas2020 for the city of Patras, south of Thessaloniki, albeit with
differences in the magnitude of OI.

An interpretation of the CE trends shows that the interaction of GHI with the clouds,
through this 30-year period, is a dynamic phenomenon that needs further
investigation. For such studies, it would be essential to have more detailed
information on cloud characteristics, especially in order to investigate possible
associations of the observed trends with changes in climate.



<!-- END -->