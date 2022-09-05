#' Ordena OD
#'
#' Função para reordenar as colunas da Observação Direta
#' @param base_od Base de dados da Observação Direta
#' @export
#' @examples
#' ordenar_od()

ordenar_od <- function(base_od){

  base_od <- base_od %>%
  # reordenando variaveis dentro da base
    select(
      formtype, geolocation, od_qa,
      nome_ue_padronizado, od_qc, od_qd,
      od_qe, od_qf, od_qg,
      od_qh, od_qi, od_qj,
      od_qk, od_ql, od_qm,
      od_qn, appliedBy, od_qo,
      od_qp, od_qq, everything(),
      od_qr
    )

  return(base_od)
}

#' Ordena EP
#'
#' Função para reordenar as colunas da Entrevista com Professores
#' @param base_ep Base de dados da Entrevista com Professores
#' @export
#' @examples
#' ordenar_ep()

ordenar_ep <- function(base_ep){

  base_ep <- base_ep %>%

    # reordenando variaveis dentro da base
    dplyr::select(
      formtype, geolocation, appliedBy,
      ep_qa, nome_ue_padronizado , ep_qc,
      ep_qd, ep_q1, ep_q2,
      ep_q3, everything(), ep_q75
    )

  return(base_ep)
}

#' Ordena ED
#'
#' Função para reordenar as colunas da Entrevista com Diretores
#' @param base_ed Base de dados da Entrevista com Diretores
#' @export
#' @examples
#' ordenar_ed()

ordenar_ed <- function(base_ed){

  base_ed <- base_ed %>%
    # reordenando variaveis dentro da base
    dplyr::select(
      formtype, appliedBy, geolocation,
      ed_qa, nome_ue_padronizado, ed_qc, everything()
    )

  # base_ed <- dplyr::select(
  #   base_ed,
  #   c(
  #     "formtype", "appliedBy", "geolocation",
  #     "ed_qa", "nome_ue_padronizado", "ed_qc", everything()
  #   )
  # )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q97_extra_1", "ed_q97_extra_2"),
    .after = ed_q97
  )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q98_extra_1", "ed_q98_extra_2"),
    .after = ed_q98
  )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q99_extra_1","ed_q99_extra_2"),
    .after = ed_q99
  )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q100_extra_1","ed_q100_extra_2"),
    .after = dplyr::ed_q100
  )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q101_extra_1","ed_q101_extra_2"),
    .after = ed_q101
  )

  return(base_ed)
}
