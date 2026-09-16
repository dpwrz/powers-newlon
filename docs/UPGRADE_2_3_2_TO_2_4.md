# Upgrade Fantasy Model 2.3.2 to 2.4

Apply the 2.4 root patch over an existing completed 2.3.2 project. Do not delete `data/`, `models/`, or `output/`.

Restart R, then run:

```r
source("TEST_2_4_PREREQS.R")
source("RESUME_2_4_FROM_SIGNAL_AUDIT.R")
```

The fast upgrade reuses the existing 2.3/2.3.2 OOF and weekly feature stores. It does **not** rebuild the expensive historical PBP store.

For a fresh project or if prerequisites are missing:

```r
source("RUN_2_4_COMPLETE.R")
```

After completion:

```r
source("RUN_APP.R")
```

Historical college and defensive-style slices are checkpointed so interrupted builds can be resumed without restarting successful slices.
