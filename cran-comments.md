## R CMD check results

0 errors | 0 warnings | 1 note

* This is an update of SHARK4R (1.2.0).

## Fix for test failure on r-devel-linux-x86_64-debian-gcc

The previous version failed its tests on the above check flavor because a test
for `run_qc_app()` did not account for Suggested packages being unavailable
during checking. On that machine the Suggested packages 'skimr' and 'plotly'
were not installed, so `run_qc_app(interactive = FALSE)` correctly raised an
error reporting the missing packages, which in turn caused the `expect_silent()`
test to fail.

The test now guards its app-dependency checks with `skip_if_not_installed()` for
the relevant Suggested packages, so it is skipped when they are absent (as
permitted by Writing R Extensions). The package's behaviour is unchanged; only
the test was affected.

## Note regarding a flagged URL

The incoming feasibility check may report the following (possibly) invalid URL:

```
URL: https://toxins.hais.ioc-unesco.org/
  Status: Error
  Message: libcurl error code 60:
      SSL certificate problem: certificate has expired
      (Status without verification: OK)
```

This URL is correct and the IOC-UNESCO Toxins Database it points to is in
regular use by the package (via `get_toxin_list()`). As the check itself notes,
the status without certificate verification is OK; the resource resolves
normally. The flag is caused solely by the remote server's TLS certificate,
which is renewed manually by the site operators and therefore occasionally
lapses for short periods. This is outside the maintainer's control and the site
returns to a valid certificate once it is renewed.

To accommodate these intermittent lapses for users, `get_toxin_list()` accepts
an opt-in `insecure = TRUE` argument that retries the download without
certificate verification, with a clear warning. Verification remains enabled by
default.
