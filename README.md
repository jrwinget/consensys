<!-- README.md is generated from README.Rmd. Please edit that file -->

# Consensys <a href="https://jrwinget-consensys.share.connect.posit.cloud"><img src="app/static/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Deployment
status](https://img.shields.io/badge/deployed-posit%20cloud-blue)](https://img.shields.io/badge/deployed-posit%20cloud-blue)
[![Code
Size](https://img.shields.io/github/languages/code-size/jrwinget/consensys)](https://github.com/jrwinget/consensys)
<!-- badges: end -->

**Consensys** is an interactive R Shiny application for exploring,
visualizing, and simulating group decision-making and judgment
processes. Based on foundational theories in group psychology—**Social
Decision Scheme (SDS)** and **Social Judgment Scheme (SJS)**—the app
helps users intuitively understand how individual inputs combine into
collective outcomes.

------------------------------------------------------------------------

## 🚀 Try the App

The app is deployed and accessible at:

🔗 [Consensys on Posit
Cloud](https://jrwinget-consensys.share.connect.posit.cloud)

------------------------------------------------------------------------

## Features

-   **Social Decision Scheme (SDS) Modeling**
    -   Visualize and experiment with classic decision rules (e.g.,
        majority, unanimity, proportionality)
    -   Customize decision matrices and explore outcome probabilities
        interactively
    -   Real-time parameter adjustments with instant visual feedback
-   **Social Judgment Scheme (SJS) Simulations**
    -   Model continuous judgments using similarity-based weights and
        confidence levels
    -   Advanced parameter controls including self-weight and decay
        parameters
    -   Explore how distance-based influence affects group consensus
    -   Conduct sensitivity analyses on weighting schemes and group
        consensus thresholds
-   **Dynamic Simulation Engine**
    -   Simulate group decision processes across rounds with real-time
        plots
    -   Real-time controls and updates during simulation
    -   Customize group size, initial positions, and aggregation
        parameters
-   **Interactive Learning Tools**
    -   Built-in guided walkthrough for new users
    -   Demo script for local exploration of decay parameters
    -   Progressive disclosure design for intuitive learning

------------------------------------------------------------------------

## Getting Started

### Online (Recommended)

Explore how groups make decisions through interactive simulations.

<figure>
<img src="man/figures/app-landing.png" alt="Consensys Landing Page" />
<figcaption aria-hidden="true">Consensys Landing Page</figcaption>
</figure>

After launching the app:

-   Click “Take A Guided Tour” for an interactive walkthrough
-   Begin with the **Quick Start Guide** on the landing page
-   Learn about SDS and SJS under the collapsible info sections
-   Choose a simulation from the sidebar:
    -   **Social Decision Scheme** to explore rule-based aggregation  
    -   **Social Judgment Scheme** to simulate confidence-weighted
        consensus  
-   Adjust parameters in real-time and see instant updates
-   Visual feedback is instant—great for teaching or insight-building

Learning time is under 10 minutes!

### Local Development

For researchers and developers wanting to explore the models locally:

    # Run the demo script to understand decay parameters
    source("man/demo_decay.R")

    # Launch the full application
    shiny::runApp()

The demo script (`man/demo_decay.R`) provides a command-line exploration
of how different decay parameters affect group consensus in SJS
simulations.

------------------------------------------------------------------------

## Advanced Features

### SJS Parameter Controls

-   **Self Weight (0-1):** Controls how much individuals rely on their
    own position versus others’ influence
-   **Decay Parameter (0.1-5.0):** Determines how quickly influence
    decreases with distance between positions
    -   Lower values (e.g., 0.5): More influence from distant positions,
        greater convergence
    -   Higher values (e.g., 5.0): Less influence from distant
        positions, slower change

### Real-Time Simulation

-   Parameters can be adjusted during simulation
-   Instant visual feedback shows how changes affect group dynamics
-   Interactive tooltips provide contextual help

### Demo Script

The included demo script (`man/demo_decay.R`) allows researchers to: -
Test different decay parameter values systematically - Compare
convergence patterns across parameter settings - Generate reproducible
results for research purposes

------------------------------------------------------------------------

## References

-   Davis, J. H. (1973). *Group decision and social interaction: A
    theory of social decision schemes*. *Psychological Review*.
-   Hinsz, V. B., Tindale, R. S., & Vollrath, D. A. (1997). *The
    emerging conceptualization of groups as information processors*.
    *Psychological Bulletin*.
-   Tindale, R. S., & Kameda, T. (2000). *‘Social sharedness’ as a
    unifying theme for information processing in groups*. *Group
    Processes & Intergroup Relations*.

------------------------------------------------------------------------

## Contributing

Feedback and contributions are welcome! Feel free to: - Open an issue
for bugs or feature requests - Submit a pull request - Share the app and
cite the original researchers who inspired it
