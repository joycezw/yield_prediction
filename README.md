# Yield Prediction Project
## To predict corn yield  based on historical yield and weather information

Specific tasks include:  
1. Literature review and quantity the effect of improvements in technology on yield responses  
2. Implement the Thompson's model (original and modified)  
3. Analyze model performance, strengths, weaknesses and effect of resolution  
4. Discuss potential model improvements  

## Predictions models and improvements   
[main.R](./main.R) contains codes that implementate prediction models and analysis yield responses.  
[utils.R](./utils.R) contains data processing functions, model implementation functions and plotting functions.  
[backup.R](./backup.R) contains codes that was not used for final results.  
[IA Data - Corn Yield - CRD.csv](./IA\ Data\ -\ Corn\ Yield\ -\ CRD.csv), [IA Data - Corn Yield - State.csv](./IA\ Data\ -\ Corn\ Yield\ -\ State.csv), [IA Data - Weather - CRD.csv](./IA\ Data\ -\ Weather\ -\ CRD.csv), [IA Data - Weather - State.csv](./IA\ Data\ -\ Weather\ -\ State.csv) contains corn yield and weather data at Crop Reporting District (CRD) and State levels collected from [National Agricultura Statistics Servise](https://www.nass.usda.gov/Charts_and_Maps/Crops_County/boundary_maps/indexgif.php).  

Five yield prediction models and improvements are implemented:  
- Original Thompson's model  
- Modified Thompson's model  
- Segmented model that conditioned on maximum June temperature  
- Prediction with bias correction sing cumulative distribution function (CDF) matching technique  
- Random Forest model  

References:  
- Thompson, L. M., (1963). Weather and technology in the production of corn and soybeans. CARD Reports, Book 17. 
http://lib.dr.iastate.edu/card_reports/17  
- Tannura, M. A., Irwin, S. H., & Good, D. L. (2008). Weather, technology, and corn and soybean yields in the US corn belt. Technology, and Corn and Soybean Yields in the US Corn Belt (February 1, 2008).  


