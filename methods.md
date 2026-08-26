# Statistical Methods

*Draft. Bracketed items marked TODO require a decision or a number from the
final dataset before circulation.*

## Study design and data sources

This is a retrospective cohort study of adults presenting to the Emergency
Department of Te Toka Tumai Auckland with severe sepsis, identified from a
clinical audit dataset and linked to national collections. Case identification
used ICD-10-AM code A41 and a structured chart review recorded in REDCap.

Linked data comprised the National Minimum Dataset (NMDS) for public hospital
admissions, the National Non-Admitted Patient Collection (NNPAC), and the
Mortality Collection for date of death. Linkage was by encrypted National Health
Index. Socioeconomic position was assigned from the four-digit health domicile
code of residence at the time of service, mapped to NZDep2023 decile using
[TODO: name the specific domicile-to-NZDep lookup and its version]. Because
NZDep2023 is constructed at Statistical Area 1 level, domicile-level assignment
introduces misclassification; domicile boundaries derive from 2001 census area
units and do not align with SA1 boundaries. Deprivation results should be read
with this in mind.

Ethnicity was assigned using the standard prioritised approach and reported at
Level 1, with European and Other combined into a single category.

## Populations

Two nested populations are analysed:

1. **All severe sepsis** — all patients meeting the pre-screening and records
   screening criteria (n = [TODO]).
2. **ARISE-eligible** — the subset additionally meeting the ARISE FLUIDS
   eligibility criteria (n = [TODO]).

Attrition from the source cohort to each population is reported in a flow
diagram following CONSORT conventions.

## Outcome

The primary outcome is days alive and out of hospital to 90 days (DAOH90),
calculated over a 90-day window beginning on the day of index ED presentation
(day 0) and ending on day 89.

Day counting follows these rules:

- Any part of a calendar day spent in hospital counts that whole day as an
  in-hospital day.
- Any part of a calendar day after death counts that whole day as dead.
- Day 0 counts as an in-hospital day.
- Death takes precedence over hospitalisation on the day of death.

Days in hospital, days dead and DAOH therefore sum to 90 for every patient.
Readmissions within the window, including those to other facilities, are
captured through NMDS and contribute to days in hospital.

## Comorbidity

Multimorbidity was quantified using the M3 index, a New Zealand index
constructed from log hazard ratios for one-year mortality across 61 chronic
conditions and validated against the Charlson and Elixhauser indices in the
national adult population.

**Limitation.** The M3 index was developed using diagnoses recorded in the five
years preceding a fixed calendar index date, in a population with no index
admission. In this study no lookback period was available, so the index was
computed from diagnoses coded during the index admission only, restricted to
conditions flagged as not arising during that episode. The resulting score is
best interpreted as comorbidity coded during the index admission rather than as
pre-existing multimorbidity. It will undercount chronic conditions not relevant
to the care of that admission, and coding depth scales with length of stay,
which is also a determinant of DAOH. M3 is therefore included as a covariate but
its coefficient is not interpreted. [TODO: retain, or seek a data amendment for
prior NMDS events]

## Descriptive analysis

Continuous variables are summarised as mean (SD) where approximately symmetric,
and median [25th, 75th percentile] where skewed; the choice was prespecified per
variable rather than made after inspection. Categorical variables are summarised
as n (%), with missing values shown as an explicit category.

For DAOH90, central tendency and dispersion are reported as the mean, standard
deviation and standard error of the mean, together with the median and the 10th,
25th, 75th and 90th percentiles. Quantiles use the median-unbiased definition
(type 8).

Confidence intervals for all DAOH summary statistics are obtained by
bias-corrected and accelerated (BCa) bootstrap with [TODO: 10,000] resamples.
Where a statistic is invariant across resamples, or where the bias-correction or
acceleration constants are not computable, no interval is reported and the cell
is marked accordingly. This occurs for the lower quantiles of DAOH because a
substantial proportion of patients have zero days at home; the proportion at
zero is reported alongside as the more informative quantity.

Summaries are presented for each population, and within each population by ARISE
eligibility and by prioritised ethnicity.

## Regression models

The distribution of DAOH is strongly left-skewed with a large point mass at zero
and a ceiling at 90. No single conditional-mean model describes it well, so two
complementary specifications are fitted.

**Linear regression** estimates differences in the conditional mean of DAOH.
The mean is the quantity a trial would target, so it is reported despite the
distributional violations; heteroscedasticity-consistent (HC3) standard errors
are used.

**Quantile regression** estimates differences in specified quantiles of the
conditional DAOH distribution. Quantiles are reported at tau = 0.50, 0.75 and
0.90 in tables and text, and across a grid of tau from 0.05 to 0.95 in figures.
Quantiles at or below the proportion of patients with zero DAOH are not
estimable, because the fitted quantile is pinned at the floor for every
covariate pattern; these are reported as not estimable rather than omitted. In
the full cohort this excludes tau up to [TODO: 0.30], and in the ARISE-eligible
subset tau up to [TODO: 0.15].

Because DAOH is heavily tied at zero, the quantile regression objective function
is degenerate. The outcome is therefore dithered by adding symmetric uniform
noise before fitting, following Machado and Santos Silva. [TODO: state the
dither width and whether estimates are averaged over multiple dithers]

A quantile contrast is a difference between the group-specific quantiles of the
outcome distribution. It is not the effect of the exposure on patients who
happen to sit at that quantile, and is not interpreted as such.

### Covariate ladder and reporting groups

Models are fitted as a sequence in which each step adds a block of covariates,
so that changes in the exposure estimate across the sequence are interpretable
as the contribution of that block:

| Model | Covariates | Rationale |
|---|---|---|
| m0 | Exposures only | Unadjusted association |
| m1 | + age, gender | Demography |
| m2 | + NZDep2023 decile, M3 index | Socioeconomic position and comorbidity |
| m3_news | + NEWS | Physiological severity at presentation |
| m3_comp | + systolic BP, heart rate, respiratory rate, temperature, SpO2, AVPU | Severity decomposed; substitutes for NEWS rather than adding to it |
| m4 | m3_news + first lactate | Perfusion, beyond what NEWS captures |
| m5 | m3_news + triage category | Care process; see triage-adjusted analysis below |

M3 enters as a restricted cubic spline with three knots, following the
recommendation of the index developers that its relationship with outcome is
not assumed linear.

Results are reported in three groups.

**Covariate ladder (primary).** Models m0 to m4, including m3_comp. This is the
main results table and figure. Because m3_comp substitutes the individual vital
signs for the NEWS summary rather than adding to it, it is presented alongside
the others for comparison rather than as a further step in the sequence.

**Severity specification.** Models m3_news and m3_comp side by side. This asks
whether the parsimonious severity adjustment (NEWS, one degree of freedom)
captures the severity information relevant to DAOH, or whether the decomposed
adjustment (six vital sign terms) changes the exposure estimate materially. A
substantial difference would indicate that NEWS is discarding information that
matters for this outcome; a negligible difference supports the parsimonious
specification, which is the preferable one at this sample size.

**Triage-adjusted analysis.** Models m3_news and m5, reported separately from
the main results. Triage category plausibly lies on the causal pathway from
ethnicity to outcome: if patients of some ethnic groups are assigned less urgent
triage categories, triage is a mechanism rather than a confounder. Adjusting for
it yields a direct effect conditional on triage, which is a different estimand
from the total effect estimated in the main analysis, and the two are not
comparable as though they were adjacent steps in the ladder. The pair is
presented so that the change on adding triage can be read directly, with the
distinction stated in the table caption. Attenuation of the ethnicity estimate
on adding triage is consistent with, but not proof of, differential triage
contributing to the difference in outcome; formal mediation analysis is not
attempted at this sample size.

The ARISE-eligible population is restricted to models m0 to m2, because the
available sample cannot support the wider specifications.

### Exposures and estimands

Two exposures are examined:

- **ARISE eligibility**, examined in the full cohort only, addressing whether the
  trial's eligibility criteria select a group with different outcomes from the
  wider severe sepsis population.
- **Prioritised ethnicity**, examined in both populations.

Exposure effects are reported as average marginal contrasts, obtained by
g-computation: predictions are generated for each patient under each exposure
level and averaged over the observed covariate distribution of the analysed
sample. This standardisation weights each patient equally, in contrast to
evaluation at a synthetic reference profile, which would give equal weight to
strata of very different size.

Ethnicity contrasts are expressed relative to the **total population average**
rather than to a single reference ethnic group, so that no group is positioned
as the standard against which others are compared.

The population average is the standardised mean prediction over the whole
analysed sample, and is therefore weighted by group size. The alternative, an
unweighted mean of the group-specific means, would give a stratum of a dozen
patients the same influence as one of a hundred and thirty-six, reintroducing
the equal-weighting problem that standardisation over the observed covariate
distribution is chosen to avoid.

Contrasts against a total-population reference are not independent of one
another: they share a comparator computed from all groups, so the deviation for
the largest group is close to zero by construction and the estimates are
correlated across groups. They are interpreted as each group's position relative
to the population, not as a set of independent pairwise comparisons.

### Uncertainty

Confidence intervals for marginal contrasts at the primary quantiles use the
bootstrap with [TODO: 1,000] resamples. These are the estimates reported in
tables and text.

Across the tau grid, which is used only to display the shape of the effect in
figures, intervals are obtained by the delta method from a bootstrapped
covariance matrix ([TODO: 500] resamples). Two approximations are involved and
are stated here rather than implied. First, the default analytic covariance for
quantile regression depends on an estimate of the sparsity function, the local
density of the response at the fitted quantile; this estimate failed for a
substantial proportion of the grid in this dataset, so a bootstrapped covariance
is used in its place. Second, a delta-method interval is a normal approximation,
and for quantile contrasts on a bounded, zero-inflated outcome at this sample
size the sampling distribution is not symmetric. Grid intervals are therefore
indicative of the shape of the effect rather than a basis for inference, and
figures distinguish them from the bootstrap intervals at the primary quantiles.

Two quantile regression diagnostics are recorded and flagged wherever the
affected estimates are reported.

A **non-unique solution** means the objective function has a flat region, so an
interval of coefficient vectors fits equally well and the reported value is one
vertex of that interval. This is expected with a heavily tied outcome. The
estimate remains a valid regression quantile, but it is not the only one, and
flagged estimates are marked in both tables and figures.

**Non-positive sparsity estimates** indicate that the local density of the
response at the fitted quantile could not be estimated for some observations.
This affects analytic standard errors, and therefore any delta-method interval
derived from them, but not the coefficient itself. It is the reason a
bootstrapped covariance is used for the grid.

### Interpretation and multiplicity

This is a feasibility study intended to characterise the population and to
inform the analysis plan of the ARISE FLUIDS trial. The regression models are
estimation rather than hypothesis testing: coefficients are reported with
confidence intervals, no adjustment is made for multiplicity, and no threshold
for statistical significance is applied. Descriptive comparisons between
populations defined by eligibility criteria are presented without p-values,
because differences in variables forming part of those criteria are guaranteed
by construction.

For the same reason, comparisons across ethnic groups are presented as
descriptive and are not adjusted for multiple comparisons. Group sizes for
Māori and Asian patients are small, and estimates for these groups carry wide
intervals; they are reported to inform trial planning and equity monitoring
rather than to support inference about ethnic differences in outcome.

### Reproducibility of randomised components

Three components of the analysis are stochastic: the dither applied to the
outcome before quantile regression, the bootstrap used for confidence intervals,
and the bootstrap used for the covariance matrices underlying the grid
intervals. All are seeded deterministically by the pipeline, so re-running
reproduces the reported values exactly. Re-running with different seeds would
produce slightly different estimates, and the magnitude of that variation is
[TODO: report the range of the exposure estimate across a small number of
independent dithers, as a sensitivity check].

## Software

Analyses were conducted in R version [TODO], using a `targets` pipeline for
reproducibility. Quantile regression used `quantreg`, marginal effects used
`marginaleffects`, bootstrapping used `boot`, and tables were produced with
`gtsummary` and `flextable`. Analysis code is available at [TODO].

## Known limitations to state

- No comorbidity lookback period (see above).
- Deprivation assigned at domicile rather than SA1 level.
- ED disposition and time to clinician assessment were not available in the
  extract; time to triage is reported in their place and is not equivalent.
- In-hospital death is captured for the index event only and will undercount
  deaths occurring during a subsequent readmission within 90 days.
- Chart review was incomplete for [TODO: n] patients, so denominators for
  treatment variables differ from those for demographic and outcome variables.