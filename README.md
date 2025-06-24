# schematic

**schematic** is an interactive R Shiny application designed to explore, visualize, and simulate group decision-making and judgment processes. Grounded in Social Decision Scheme (SDS) and Social Judgment Scheme (SJS) theories by Davis, Tindale, and Hinsz, this app enables users to intuitively understand how groups combine individual preferences and judgments into collective outcomes.

---

## Features

- **Social Decision Scheme (SDS) Modeling**
  - Visualize and experiment with various decision rules (e.g., majority, unanimity, proportionality).
  - Customize decision matrices and explore outcome probabilities interactively.

- **Social Judgment Scheme (SJS) Simulations**
  - Model continuous judgments with weighted individual inputs based on similarity and confidence.
  - Conduct sensitivity analyses on decision weights and group consensus.

- **Dynamic Simulations**
  - Run customizable group decision-making scenarios to explore impacts of group composition and decision rules.

---

## Getting Started

### Installation

Clone this repository:

```bash
git clone https://github.com/jrwinget/schematic.git
```

### Running the app
Ensure you have installed the necessary dependencies:

```r
renv::restore(clean = TRUE)
```

Launch the app directly from R:

```r
# from the project directory
rhino::app()
```

---

## References

- Davis, J. H. (1973). Group decision and social interaction: A theory of social decision schemes. Psychological Review. 
- Tindale, R. S., & Kameda, T. (2000). 'Social sharedness' as a unifying theme for information processing in groups. Group Processes & Intergroup Relations. 
- Hinsz, V. B., Tindale, R. S., & Vollrath, D. A. (1997). The emerging conceptualization of groups as information processors. Psychological Bulletin. 

---

## Contributing

Feedback, issues, and contributions are welcome! Please open an issue or submit a pull request.
