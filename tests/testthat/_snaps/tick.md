# tick works (external package, ticker interface)

    Code
      lm_ticker
    Message
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

