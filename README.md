# conditionals
This repository contains the code for the project on probabilistic modeling of
indicative conditionals with the Rational-Speech-Act model.
Code for two behavioral experiments can be found in two separate repositories:
[Prior experiment](https://github.com/brittaGrusdt/conditionals-pilot) and [Main experiment](https://github.com/brittaGrusdt/blocksworld-main).

## Project structure

```
|--data/
|    |--default-model/
|    |   |--figs/
|    |
|    |--douven-examples/
|       |--figs/
|
|--model/
|   |--default-model/
|   |--douven-examples/
|
|--node_modules/     
|   |--conditionalsDefault/
|   |--conditionalsHelpers/
|
|--R/
|  |--default-model/
|  |--douven-examples/
|  |--plotting/
```

* data
  * stores model predictions for respective model as .rds objects
  * and respective figures in subfolder /figs/

* model
  * contains webppl code for respective model setting up the respective
    contexts, e.g. the prior
  * containts, if any, webppl files for generating utterances, causal nets etc.

* node_modules
  * /conditionalsDefault/

      contains the definition for the prior in default context since it's
      already called before running the model,
      e.g. to check whether there are utterances that do not have a
      corresponding state (depending on context)

  * /conditionalsHelpers/

      contains rsa model util functions, and helper functions

* R
  * contains R code for respective model to: run the model (calling the
    webppl files),
    create tables etc.
  * /plotting/
    contains code to make figures
