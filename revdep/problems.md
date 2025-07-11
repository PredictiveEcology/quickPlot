# SpaDES.core

<details>

* Version: 2.1.5.9003
* GitHub: https://github.com/PredictiveEcology/SpaDES.core
* Source code: https://github.com/cran/SpaDES.core
* Number of recursive dependencies: 157

Run `revdepcheck::revdep_details(, "SpaDES.core")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘test-all.R’
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      ── Error ('test-1memory.R:36:5'): testing memoryUse ────────────────────────────
      Error in `memoryUseSetup(sim, originalPlan)`: memoryUse encountered an error; perhaps logPath(sim) not writable?
      Backtrace:
          ▆
       1. ├─SpaDES.core::spades(mySim2, debug = TRUE) at test-1memory.R:36:5
       2. ├─SpaDES.core::spades(mySim2, debug = TRUE)
       3. │ ├─base::withCallingHandlers(...)
       4. │ └─SpaDES.core:::memoryUseSetup(sim, originalPlan)
       5. │   └─base::stop("memoryUse encountered an error; perhaps logPath(sim) not writable?")
       6. └─base::.handleSimpleError(...)
       7.   └─SpaDES.core (local) h(simpleError(msg, call))
      
      [ FAIL 1 | WARN 0 | SKIP 9 | PASS 794 ]
      Error: Test failures
      Execution halted
    ```

