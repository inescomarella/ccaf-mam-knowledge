#' Function to fit glm to mammal records
#'
#' @param data data.frame containing the columns: nrec, proximity, CU, forestw
#'
#' @return
#'
#' @export
#'
#' @examples

model_selection <- function(data) {
  
  # Full: All covariates
  m_full <- glm(nrec ~ proximity + CU + forestw,
                family = poisson,
                data = data
  )
  # Only covariates related with accessibility
  m_1 <- glm(nrec ~ proximity + CU,
             family = poisson,
             data = data
  )
  # Only the effect of collection proximity and relative forest cover
  m_2 <- glm(nrec ~ proximity + forestw,
             family = poisson,
             data = data
  )
  # Only the effect of proximity
  m_3 <- glm(nrec ~ proximity,
             family = poisson,
             data = data
  )
  # Null model
  m_4 <- glm(nrec ~ 1,
             family = poisson,
             data = data
  )

  ## Rank models by AIC
  res <- list(
    modelos = list(
      m_full = m_full,
      m_1 = m_1, m_2 = m_2, 
      m_3 = m_3, m_4 = m_4
    ),
    aic_tab = AICctab(m_full, m_1, m_2, m_3, m_4, base = TRUE, weights = TRUE)
  )
  return(res)
}
