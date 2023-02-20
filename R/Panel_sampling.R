#' @importFrom dplyr group_by 
#' @importFrom dplyr summarise 
#' @importFrom dplyr left_join  
#' @importFrom dplyr filter  
#' @importFrom dplyr pull   
#' @export
#' 
#' @title
#' Assigns survey units to panels and samples units in each panel
#' @description
#' This function assigns survey units (i.e., PSUs) to panels and samples a specified
#'  number of units in each panel using a rotating panel design. The input is a 
#'  data frame with stratification and PSU variables, a panel design, the number of 
#'  PSUs and sample size by stratum.
#' @return A list with the following elements:
#'  \describe{
#'   \item{DF_Strata_Panels}{A \code{data.frame} of all PSUs, with each PSU assigned to a panel and its population and sample sizes assigned to a panel}
#'   \item{DF_Size_Pob_Panels}{A \code{data.frame} with the total number of PSUs in each panel (in the population)}
#'   \item{DF_Size_Sample_Panels}{A \code{data.frame} with the number of PSUs in each panel (in the sample)}
#'   \item{Panel_structure}{A vector with the details of panel composition}
#'   \item{Panel_structure_sample}{A vector with the details of PSU selection in each panel}
#'   \item{Checked_PSU_stratum_size}{A vector with a check of the correct assignment of PSUs to panels such that the number of PSUs per stratum is respected}
#'   \item{Checked_sample_size_PSU_strata}{A vector with a check of the correct PSU selection in panels such that the sample size of PSUs in each stratum is respected}
#'   }
#' @author Jose Fernando Zea Castro <jfzeac at unal.edu.co>, Hugo Andres Gutierrez Rojas <andres.gutierrez at cepal.org>, Stalyn Yasid Guerrero Gomez <syguerrerog at unal.edu.co>   
#' @param DF \code{data.frame} with the columns 'stratum_column' and 'PSU_column'.
#' @param stratum_column The name of the column containing the stratum identifiers in `DF`
#' @param PSU_column The name of the column containing the PSU identifiers in `DF`
#' @param panels A \code{data.frame} containing the panel structure
#' @param PSU_stratum_size The number of PSUs in each stratum (in the population)
#' @param PSU_stratum_sample_size The number of PSUs to select in each stratum-panel combination (in the sample)
#' @param seed The seed to use for the random number generator
#' @seealso \code{\link{Redistribution_PSU}}, 
#' @seealso \code{\link{Redistribution_PSU_sample}}
#' @examples 
#' data(Data_PSU)
#' data(Data_PSU_aggr)
#' paneles <- cbind(Rotating_panels(A = 5, B = 0, C = 0, period = 13*4), 
#' Rotating_panels(A = 5, B = 0, C = 0, period = 13*4, value_initial = "F"),
#' Rotating_panels(A = 5, B = 0, C = 0, period = 13*4, value_initial = "K"))
#' 
#' stratum <- "111"
#' 
#' dt <- Data_PSU %>% filter(straum_id == stratum)
#' Num_PSU_strata <-
#'   2 * Data_PSU_aggr$N_PSU[Data_PSU_aggr$straum_id ==  stratum]
#' 
#' sample_size_PSU_stratum <-
#'   Data_PSU_aggr$n_PSU[Data_PSU_aggr$straum_id ==  stratum]
#' 
#' Panel_sampling(
#'   DF = dt,
#'   stratum_column = "straum_id",
#'   PSU_column = "PSU_id",
#'   panels = paneles ,
#'   PSU_stratum_size = Num_PSU_strata,
#'   PSU_stratum_sample_size = sample_size_PSU_stratum,
#'   seed = 12345
#' ) 




Panel_sampling <- function(DF,
                           stratum_column,
                           PSU_column,
                           panels ,
                           PSU_stratum_size,
                           PSU_stratum_sample_size,
                           seed = 12345) {
  
  if (!is.data.frame(DF)) {
    stop("Error: The first argument must be a dataframe.")
  }
  
  if (!is.character(stratum_column) || !is.character(PSU_column)) {
    stop("Error: Stratum and PSU column names must be characters.")
  }
  
  if (!(is.matrix(panels) || is.data.frame(panels))) {
    stop("Error: The panel argument must be an matrix")
  }
  
  if (!is.numeric(PSU_stratum_size) ||
      any(PSU_stratum_size < 1) ||
      length(PSU_stratum_size) != length(unique(DF[[stratum_column]]))) {
    stop(
      "Error: argument PSU_stratum_size must be a numeric vector with the number of PSUs per stratum."
    )
  }
  
  if (!is.numeric(PSU_stratum_sample_size) ||
      any(PSU_stratum_sample_size < 1) ||
      length(PSU_stratum_sample_size) != length(unique(DF[[stratum_column]]))) {
    stop(
      "Error: PSU_stratum_sample_size must be a numeric vector with sample size per PSU and stratum."
    )
  }
  
  
  DF$stratum <- DF[[stratum_column]]
  DF$PSU <- DF[[PSU_column]]
  
  cod_panels <- sort(as.matrix(panels) %>% as.vector() %>% unique())
  total_num_panels <-
    panels %>% as.matrix() %>% as.vector() %>% unique() %>% length()
  
  num_panels_period <- ncol(panels)
  
  set.seed(seed)
  DF$random <- runif(n = nrow(DF))
  DF <- arrange(DF, random)
  info <- Redistribution_PSU(PSU_stratum_size, total_num_panels)
  info <- c(info, "Size_PSU_stratum" = nrow(DF))
  
  
    if (info["Num_large_panels"] != 0) {
    psu1 <- rep(cod_panels[1:info["Num_large_panels"]],
                rep(info["Num_PSU_large_panels"], info["Num_large_panels"]))
    
    psu2 <-
      rep(cod_panels[(info["Num_large_panels"] + 1):(info["Num_large_panels"] + info["Num_normal_panels"])],
          rep(info["Num_PSU_normal_panels"], info["Num_normal_panels"]))
  }   else {
    psu1 <- NULL
    psu2 <- rep(cod_panels[1:info["Num_normal_panels"]],
                rep(info["Num_PSU_normal_panels"],
                    info["Num_normal_panels"]))
  }
  
  DF$Panels <- c(psu1, psu2)
  size_Pob <-
    DF %>% group_by(Panels) %>% summarise(NpobPSUPanels = n())
  DF <- DF %>% group_by(Panels) %>% mutate(NpobPSUPanels = n())
  
  
  info_mue <-
    Redistribution_PSU_sample(PSU_stratum_sample_size, num_panels_period)
  info_mue <-
    c(info_mue, "PSU_stratum_sample_size" = PSU_stratum_sample_size)
  
  if (info_mue["Num_large_panels"] != 0) {
    id_selPanelsLarge <-
      sample(ncol(panels), info_mue["Num_large_panels"])
    psu1_mue <-
      panels[, id_selPanelsLarge]  # 1:info_mue["Num_large_panels"]
    psu1_mue <- psu1_mue %>% as.matrix() %>% as.vector() %>% unique()
    
    id_SelPanelsMedium <-
      1:(info_mue["Num_large_panels"] + info_mue["Num_normal_panels"])
    id_SelPanelsMedium <-
      setdiff(id_SelPanelsMedium, id_selPanelsLarge)
    psu2_mue <-
      panels[, id_SelPanelsMedium] %>% as.matrix() %>% as.vector() %>% unique()
    
    rep_psu1_mue <-
      rep(psu1_mue, rep(info_mue["Num_PSU_large_panels"] %>% as.numeric(), length(psu1_mue)))
    rep_psu2_mue <-
      rep(psu2_mue, rep(info_mue["Num_PSU_normal_panels"] %>% as.numeric(), length(psu2_mue)))
    
    rep_upm_mue <- c(rep_psu1_mue, rep_psu2_mue)
    DF_Size_Sample_Panels <- table(rep_upm_mue)
    DF_Size_Sample_Panels <- DF_Size_Sample_Panels %>% as.data.frame()
    names(DF_Size_Sample_Panels) <- c("Panels", "nPSUPanels")
  } else  {
    psu2_mue <- panels %>% as.matrix() %>% as.vector() %>% unique()
    
    rep_psu1_mue <- NULL
    rep_psu2_mue <-
      rep(psu2_mue, rep(info_mue["Num_PSU_normal_panels"] %>% as.numeric(), length(psu2_mue)))
    
    rep_upm_mue <- c(rep_psu1_mue, rep_psu2_mue)
    DF_Size_Sample_Panels <- table(rep_upm_mue)
    DF_Size_Sample_Panels <- DF_Size_Sample_Panels %>% as.data.frame()
    names(DF_Size_Sample_Panels) <- c("Panels", "nPSUPanels")
  }
  
  DF <- left_join(DF, DF_Size_Sample_Panels, by = "Panels")
  
  set.seed(seed)
  DF$random_sel <- runif(n = nrow(DF))
  DF <- DF %>% arrange(stratum, Panels, random_sel)
  DF <- DF %>% group_by(stratum, Panels) %>% mutate(secuencia = 1:n())
  DF$Sel_PSU <- as.numeric(DF$nPSUPanels >= DF$secuencia)
  
  size_Pob$stratum <- unique(DF$stratum)
  DF_Size_Sample_Panels$stratum <- unique(DF$stratum)
  
  size_Pob <- size_Pob[c("stratum", "Panels", "NpobPSUPanels")]
  DF_Size_Sample_Panels <-
    DF_Size_Sample_Panels[c("stratum", "Panels", "nPSUPanels")]
  
  
  info <- c("stratum" = unique(DF$stratum), info)
  info_mue <- c("stratum" = unique(DF$stratum), info_mue)

  if (PSU_stratum_size == as.numeric(info[names(info) == "Size_PSU_stratum"]) &
      PSU_stratum_size == nrow(DF)  &
      PSU_stratum_size ==
      DF %>% filter(Panels %in% as.character(unique(
        panels %>% as.matrix() %>% as.character()
      ))) %>%
      group_by(Panels) %>%
      summarise(NpobPSUPanels = max(NpobPSUPanels)) %>% pull(NpobPSUPanels) %>% sum()) {
    chequeo_PSU_stratum_size <- "OK"
  } else {
    chequeo_PSU_stratum_size <- "Error_num_PSU_stratum"
  }
  
  
  
  check_Size_sample_PSU_stratum <- nrow(panels)
  for (k in 1:nrow(panels)) {
    check_Size_sample_PSU_stratum[k] <-
      DF %>% filter(Panels %in% as.character(panels[k, ])) %>% group_by(Panels) %>%
      summarise(nPSUPanels = max(nPSUPanels)) %>% pull(nPSUPanels) %>% sum() == PSU_stratum_sample_size
  }
  
  if (sum(check_Size_sample_PSU_stratum) == nrow(panels)) {
    chequeo_sample_size_PSU_strata <- "OK"
  } else {
    chequeo_sample_size_PSU_strata <- "Error_PSU_stratum_sample_size"
  }
 
  
  result <- list(
    DF,
    size_Pob,
    DF_Size_Sample_Panels,
    info,
    info_mue,
    chequeo_PSU_stratum_size,
    chequeo_sample_size_PSU_strata
  )
  names(result) <- c(
    "DF_Strata_Panels",
    "DF_Size_Pob_Panels",
    "DF_Size_Sample_Panels",
    "Panel_structure",
    "Panel_structure_sample",
    "Checked_PSU_stratum_size",
    "Checked_sample_size_PSU_strata"
  )
  return(result)
}
