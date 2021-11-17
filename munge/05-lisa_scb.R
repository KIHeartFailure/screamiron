rsdata <- rsdata %>%
  mutate(scbyear = shf_indexyear - 1)

rsdata <- left_join(
  rsdata,
  lisa,
  by = c("LopNr" = "LopNr", "scbyear" = "year")
) %>%
  select(-scbyear)

## income
inc <- rsdata %>%
  group_by(LopNr, shf_indexyear) %>%
  slice(1) %>%
  ungroup () %>%
  group_by(shf_indexyear) %>%
  summarise(incsum = list(enframe(quantile(scb_dispincome,
                                           probs = c(0.33, 0.66),
                                           na.rm = TRUE
  )))) %>%
  unnest(cols = c(incsum)) %>%
  spread(name, value)

rsdata <- left_join(
  rsdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat = case_when(
      scb_dispincome < `33%` ~ 1,
      scb_dispincome < `66%` ~ 2,
      scb_dispincome >= `66%` ~ 3
    ),
    scb_dispincome_cat = factor(scb_dispincome_cat, labels = c("Low", "Medium", "High"))
  ) %>%
  select(-`33%`, -`66%`)

