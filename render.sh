#!/usr/bin/env sh
# Renders the two languages of the site. The order matters: the English render
# cleans docs/, so the Portuguese one (docs/pt/) must come after it.
quarto render --profile en && quarto render --profile pt
