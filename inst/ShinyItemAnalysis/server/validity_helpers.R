# Pure helpers for the Validity tab. Kept free of Shiny reactives so they can
# be unit tested directly (see tests/manual/test-validity-helpers.R).

# ** Which IRT model provides θ for a given data type ####
# The Data tab records the type of the items *as uploaded* ("binary",
# "nominal", "ordinal", "continuous"), which is not the same question as which
# IRT model applies to them. Nominal responses are scored against the key with
# mirt::key2binary before any analysis, so they are handled by the dichotomous
# model exactly as binary data are — see the nominal branch of the toy-data
# observer in server/Data.R, which fills dataset$binary from key2binary().
# Returns "dichotomous", "polytomous", or NA when θ does not apply at all.
.validity_theta_model <- function(dtype) {
  if (identical(dtype, "binary") || identical(dtype, "nominal")) {
    return("dichotomous")
  }
  if (identical(dtype, "ordinal")) {
    return("polytomous")
  }
  NA_character_
}
