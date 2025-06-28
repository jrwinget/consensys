# schematic

**schematic** is an interactive R Shiny application for exploring, visualizing, and simulating group decision-making and judgment processes. Based on foundational theories in group psychology (SDS and SJS), the app helps users intuitively understand how individual inputs combine into collective outcomes.

*Coming Soon:* This app will be hosted via GitHub Pages directly from the repository.

---

## Features

- **Social Decision Scheme (SDS) Modeling**
  - Visualize and experiment with classic decision rules (e.g., majority, unanimity, proportionality).
  - Customize decision matrices and explore outcome probabilities interactively.

- **Social Judgment Scheme (SJS) Simulations**
  - Model continuous judgments using similarity-based weights and confidence levels.
  - Conduct sensitivity analyses on weighting schemes and group consensus thresholds.

- **Dynamic Simulation Engine**
  - Simulate group decision processes across rounds with real-time plots.
  - Customize group size, initial positions, and aggregation parameters.

---

## Getting Started

### 1. Clone the repository

```bash
git clone https://github.com/jrwinget/schematic.git
```

### 2. Install dependencies

You’ll need R, RStudio (or Posit), and a working `{rhino}` setup. Then, in R:

```r
renv::restore()
```

### 3. Run the app

```r
rhino::app()
```

---

## References

- Davis, J. H. (1973). *Group decision and social interaction: A theory of social decision schemes*. *Psychological Review*.
- Hinsz, V. B., Tindale, R. S., & Vollrath, D. A. (1997). *The emerging conceptualization of groups as information processors*. *Psychological Bulletin*.
- Tindale, R. S., & Kameda, T. (2000). *‘Social sharedness’ as a unifying theme for information processing in groups*. *Group Processes & Intergroup Relations*.

---

## Contributing

Feedback and contributions are welcome! Feel free to:
- Open an issue for bugs or feature requests
- Submit a pull request
- Share the app and cite the original researchers who inspired it
