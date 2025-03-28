# Methods

## Study Design

This study employed a 5 (Context: medical, legal, financial, psychological,
educational) × 2 (Stakes: high vs. low) × 2 (Interaction Partner: AIES vs.
human) mixed design. Context served as a between-subjects factor, while both
stakes and interaction partner were manipulated within-subjects. The study was
preregistered on the OSF ([OSF link placeholder]) and a priori power analyses
confirmed sufficient power to detect interaction effects. Specifically, the
power for the predictor 'Context × Partner' was 99.60% (95% CI: 99.21, 99.83),
based on 2,000 simulations using a Type-2 F-test with Satterthwaite degrees of
freedom (alpha = .05, n = 3,592).

## Participants

A total of 898 UK participants ($M_age = 42$, $SD = 14$; 50.6% female; 36% with
a high-school diploma or equivalent, 62% with a Bachelor’s degree or higher)
were recruited via the online platform Prolific. Participants were randomly
assigned to one of the five context conditions, with approximately 180
individuals per group.

## Procedure

Participants first provided informed consent and completed a set of background
measures, including general attitudes toward AI, subjective and objective AI
knowledge, prior AI experience, and general propensity to trust. Subsequently,
they were presented with the introduction to AIES:

"An AI expert system is an artificial intelligence system that can do work
similar to that of human professionals such as lawyers, doctors,
psychotherapists, tax accountants, or teachers. It is a computer program that
uses knowledge and rules to help solve problems in a specific area. It mimics
the expertise of a human expert and can give advice and make decisions based on
the information it has learned. It's like having a digital expert in a
particular field."

However, to make an informed decision about whether to trust AIES—and to
evaluate their perceived risk and trustworthiness—a concise set of additional
descriptive elements was provided. These elements were necessary to control for
the many relevant factors that might otherwise remain unaddressed. The final
list of descriptive elements stated that an AIES

* “is approved by an official governmental assessment procedure;
* guarantees regulatory compliance; ensures ethical behavior;
* guarantees the protection of user privacy (including user data);
* indicates the accuracy and reliability of external data sources;
* is fully transparent about its decision-making procedure;
* provides full transparency about all costs before they occur;
* and informs truthfully about the company that created it and its country of
  origin.”

The survey instruments and descriptive elements were calibrated and validated
during preliminary studies (S1–S5; see Sup for more information). Afterward,
participants were randomly assigned to one of five professional context
conditions and exposed to two matched scenarios (low-stakes and high-stakes),
presented in counterbalanced order. Each scenario asked participants to compare
a human expert and an AIES in that context.

## Measures

Perceived Trustworthiness was measured using the nine-item short version of the
Münster Epistemic Trustworthiness Inventory (METI; [REF]), assessing the
dimensions of expertise, integrity, and benevolence on 7-point bipolar scales
(e.g., incompetent–competent).

Perceived Risk and Trust (willingness to rely) were each assessed using a
single-item 7-point scale ("None at all" to "Maximally")."
 

# Results

Hier noch preliminär und noch nicht abgesegent ein Teil der Analyses und
Results: 
 
## Linear Mixed-Effects Models (LMMs) for Trustworthiness and Risk

To test H1 and H2, we conducted two linear mixed-effects models predicting
perceived trustworthiness and perceived risk, respectively. Fixed effects
included Context (reference: educational), Partner (reference: AIES), Stakes
(reference: high), and their interactions. Random intercepts and slopes for
Partner and Stakes were modeled at the participant level. Full model estimates,
including significance levels and effect sizes, are reported in Table 1.

### Trustworthiness

As hypothesized (H1), the model revealed a significant main effect of Partner,
with human professionals rated significantly higher in trustworthiness than AIES
(p < .001, η²partial = .43). Only the psychological context deviated
significantly from the educational reference, with lower overall trustworthiness
ratings (p = .0002). Context × Partner interactions were significant in the
medical (β = 0.532, p = .00031, η²partial = .03) and psychological (β = 0.570,
p < .001, η²partial = .03) domains, indicating that the partner effect was
especially pronounced in these contexts. Additionally, a three-way interaction
between Context, Partner, and Stakes emerged in the medical domain (β = -0.409,
p < .001, η²partial = .01), where AIES trustworthiness declined under
high-stakes conditions while human ratings remained stable. These estimates
suggest that although context exerts an effect, it is primarily driven by the
psychological condition, whereas partner type is the more consistent predictor
across settings. The effect of Stakes alone (η²partial = .01) and as evidenced
by the reported effect sizes was minimal. These findings directly address RQ3 by
demonstrating that professional context exerts a greater influence on
trustworthiness evaluations than the stakes involved. As seen in Figure 1 (top
panel), trustworthiness ratings for human experts (blue line) consistently
exceed those for AIES (orange line) across all contexts and stakes. The
psychological and medical domains under high-stakes conditions show particularly
large gaps, aligning with the observed interaction effects.

### Risk

The model for perceived risk confirmed H1b: AIES were generally seen as riskier
than human experts (β = -0.864, p < .001). Context had a significant influence,
with higher risk ratings in the legal (β = 0.542, p = .002), medical (β = 0.636,
p < .001), and psychological (β = 0.493, p = .005) domains compared to the
educational reference. A significant main effect of Stakes (β = -0.542, p <
.001) and two notable Context × Stakes interactions—specifically in the legal (β
= -0.367, p = .032) and medical (β = -1.158, p < .001) domains—further supported
H2. These results indicate that higher stakes amplify perceived risk primarily
in these high-consequence domains. As shown in Figure 1 (bottom panel), risk
evaluations diverge more across contexts than trustworthiness. Visual inspection
highlights consistently higher risk ratings for AIES (orange line) than for
human experts (blue line), particularly in legal and psychological settings
under high-stakes conditions. A notable crossover under low stakes in the
medical domain further illustrates the contextual sensitivity of risk
perceptions.

When visually comparing the upper and lower panels of Figure 1, it becomes
apparent that the gap between AIES and human professionals is consistently
larger for trustworthiness than for risk. Moreover, while trustworthiness
ratings remain relatively stable across stakes, risk perceptions exhibit more
pronounced shifts between high- and low-stakes conditions, particularly for
AIES. This visual pattern reinforces the finding that trustworthiness is
evaluated more consistently, whereas risk assessments are more sensitive to
contextual cues and situational framing.

## Bayesian Multivariate Model

To complement our LMMs, we fit a Bayesian multivariate linear mixed model using
the brms package. Trustworthiness and risk were modeled jointly to assess their
relationship beyond shared predictors, with both outcomes sharing random
intercepts and slopes for Partner and Stakes at the participant level. The model
allowed residuals for trustworthiness and risk to correlate, offering insight
into their relationship beyond shared predictors. Convergence diagnostics were
good (Rhat ≈ 1 across parameters), and posterior estimates aligned with the LMM
results: human experts were rated more trustworthy (β = 1.03, 95% CI [0.82,
1.23]) and less risky (β = –0.87, 95% CI [–1.15, –0.58]) than AIES. A residual
correlation of r = –0.29 (95% CI [–0.34, –0.24]) was observed, indicating that
participants who rated an expert as more trustworthy tended to also perceive
less risk.

This finding is noteworthy, as perceived trustworthiness and perceived risk are
theoretically independent—individuals can judge a trustee as highly trustworthy
even in contexts with elevated risk (e.g., high-stakes medical scenarios), or
perceive little risk while still regarding the trustee as untrustworthy. The
Mayer et al. (1995 [REF]) framework implies that trust must increase to offset
risk for reliance to occur, but this mechanism pertains to how trust is
calibrated, not how trustworthiness is judged.  We return to this point in the
discussion section to further unpack the implications of this finding. 
 
