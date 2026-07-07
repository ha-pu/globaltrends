# Force untranslated (English) condition messages for the whole test run so
# message/error assertions are locale-independent. Restored automatically when
# the suite finishes via teardown_env().
withr::local_envvar(c(LANGUAGE = "EN"), .local_envir = teardown_env())
