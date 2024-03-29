# tick works (external package, ticker interface)

    Code
      lm_ticker
    Output
      A <ticker> for `stats::lm()`.

---

    Code
      stats::lm
    Output

---

    Code
      untick(lm_ticker)
    Condition
      Error in `untick()`:
      ! Could not find active ticker for `stats::lm`.

---

    Code
      ticks(lm_ticker)
    Condition
      Error in `ticks()`:
      ! Could not find active ticker for `stats::lm`.

---

    Code
      lm_ticker
    Output
      A <ticker> for `stats::lm()`.
      x The ticker is no longer active!

# tick works (external package, x + pkg interface)

    Code
      untick("lm", "stats")
    Condition
      Error in `untick()`:
      ! Could not find active ticker for `fn` lm and `pkg` stats.

---

    Code
      ticks("lm", "stats")
    Condition
      Error in `ticks()`:
      ! Could not find active ticker for `fn` lm and `pkg` stats.

