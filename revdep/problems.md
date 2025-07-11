# SpaDES.core

<details>

* Version: 2.1.0
* GitHub: https://github.com/PredictiveEcology/SpaDES.core
* Source code: https://github.com/cran/SpaDES.core
* Date/Publication: 2024-06-02 11:02:47 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::revdep_details(, "SpaDES.core")` for more info

</details>

## In both

*   checking running R code from vignettes ...
    ```
      ‘i-introduction.Rmd’ using ‘UTF-8’... OK
      ‘ii-modules.Rmd’ using ‘UTF-8’... failed
      ‘iii-cache.Rmd’ using ‘UTF-8’... failed
      ‘iv-advanced.Rmd’ using ‘UTF-8’... OK
      ‘v-automated-testing.Rmd’ using ‘UTF-8’... OK
     ERROR
    Errors in running code in vignettes:
    when running code in ‘ii-modules.Rmd’
      ...
    restartSpades()
    ...
    The initial state of the last1events are cached and savedin the simList located at savedSimEnv()$.sim,as sim$.recoverableObjs, with the most recent eventthe first element in the list, 2nd most recent event = the second most recent event, etc. The objects contained in each of those are only the objects that may havechanged, according to the metadata for each module. To recover, use:cli-631531-1
    Because of an interrupted spades call, the sim object at the start of the interrupted event was saved in
    SpaDES.core:::savedSimEnv()$.sim
    It will be deleted on next call to spades().
    Timing stopped at: 0.159 0.015 0.179
    
      When sourcing ‘iii-cache.R’:
    Error: Package 'NLMR' not available. Please install it using:
      install.packages('NLMR', repos = 'https://predictiveecology.r-universe.dev')
    Execution halted
    ```

