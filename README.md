# Goals 

The `the-smart-designer` library presents the user with a blaze-html based interface to the [Smart designing system](https://design.smart.coop/development/docs/). 

This project also has an executable, the goal of which is to recreate the page https://design.smart.coop/development/docs/ using the library as a proof of concept. 

## The structure

This is a cabal project, with two packages: 
- `the-smart-designer`: the library itself. 
- `the-smart-designer-exe`: the executable server that provides us with a page like https://design.smart.coop/development/docs/, but using the lib. 

## Notes on ease of development. 

To keep the compilation and test costs low at this point; I'll take the liberty to put the exe into the library package and we'll split this out as things mature. 

