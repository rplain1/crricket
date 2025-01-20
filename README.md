# crricket

If you are reading this, thank you. 

## Overview

Refactor https://github.com/rplain1/cricket to work in R. 

## Run Pipeline and make predictions

```
git clone https://github.com/rplain1/crricket.git

cd crricket || exit

docker build -t crricket .

docker run --rm crricket
```

## Status

- This was an attempt to get the code up and running in R as quickly as possible. My initial goal was to make an R package that was independent of the data sources and potentially submittable to CRAN. 
- Doing this caused design limitations, most notably using temporary files for model building. This caused issues to implement in testing with the lack of a model object. To build out the tests.
- Additionally, I used the same method in the python version of parsing the JSON with `duckdb`. This used parquet and added a dependency to `arrow`. 
- Running the docker command, currently is building everything from source with `pak`, which is getting hung up on the build for `arrow`. This isn't a bug I have encountered before, and took a while to debug. Next steps would be to continue to troubleshoot and get `pak` working correctly, or remove the `arrow` dependency. 

