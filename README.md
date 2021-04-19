## Final Year Project / Honors Dissertation (BHD4001)
<b><i>Markov Text Generation Comparison App</i></b>

Part of my Honors Dissertation dealing with Singlish (Singaporean colloquial English).

For this part of the project, six discrete time Markov models were trained on Singlish data. Each model treats a group of n words as a state (from 1-gram/unigram to 6-gram) and from there a sentence can be generated through probabilistic transitions between states (i.e. selecting the state transition with the highest probability). 

This shiny application demonstrates the differences in sentence generations between the different models trained in the project.

Data comes from a collection of 29,656 unique sentences, of which 26,690 was sampled to "train" the Markov model.

The app can be accessed on Shinyapps: https://kaiwei-tan.shinyapps.io/MarkovTextGenerationComparison/

Supervised by: Professor Yuan Xue-Ming
