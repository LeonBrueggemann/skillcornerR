# skillcornerR

`skillcornerR` is an R package to interact with the **SkillCorner API** that handles pagination automatically. It provides methods to retrieve and save various data related to competitions, seasons, matches, teams, and players.

---

## Installation

You can install the development version of `skillcornerR` directly from GitHub using either `devtools` or `remotes`.

### Option A: Using devtools (Recommended)
```R
# 1. Install devtools if you don't have it already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# 2. Install the package from GitHub
devtools::install_github("LeonBrueggemann/skillcornerR")

```

### Option B: Using remotes

```R
# 1. Install remotes if you don't have it already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# 2. Install the package from GitHub
remotes::install_github("LeonBrueggemann/skillcornerR")

```

---

## Authentication Note

The SkillCorner API requires basic authentication. All functions in this package require your API `username` and `password` as their first two arguments.

### Best Practice Example:

```R
library(skillcornerR)

# Define your credentials safely
my_user <- "your_username"
my_pass <- "your_password"

# Fetch all available competitions
competitions <- get_skc_competitions(username = my_user, password = my_pass)

```

---

## Quick Start & Function Overview

Once loaded, you can check the documentation and parameters for any function directly inside RStudio by typing a question mark before the function name (e.g., `?get_skc_physical`).

### 1. General Metadata & Competitions

* `get_skc_competitions()`
* `get_skc_competition_editions()`
* `get_skc_editions()`
* `get_skc_rounds()`
* `get_skc_match()`

### 2. Raw Tracking & Physical Aggregations

* `get_skc_tracking()`
* `get_skc_physical()`

### 3. Dynamic Events

* `get_skc_dynamic_events()`
* `get_skc_dynamic_events_off_ball_runs()`
* `get_skc_dynamic_events_on_ball_engagements()`
* `get_skc_dynamic_events_passing_options()`
* `get_skc_dynamic_events_phases_of_play()`
* `get_skc_dynamic_events_player_possessions()`

### 4. Raw Endpoints - full breadth match by match metrics datasets

* `get_skc_match_metrics_off_ball_runs()`
* `get_skc_match_metrics_passes()`
* `get_skc_match_metrics_passing_options()`
* `get_skc_match_metrics_player_possessions()`
* `get_skc_match_metrics_on_ball_engagements()`

### 5. Intelligent Endpoints - subset of metrics with computation capabilities

* `get_skc_metrics_off_ball_runs()`
* `get_skc_metrics_passes()`
* `get_skc_metrics_passing_options()`
* `get_skc_metrics_player_possessions()`
* `get_skc_metrics_on_ball_engagements()`

---
