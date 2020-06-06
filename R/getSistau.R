# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @title getSistau
#' @description Get SISTAU data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @details
#' @rdname getSistau
#' @author Juan Ignacio Fulponi
#' @source Ministerio de Obras Públicas y Ministerio de Transporte de la Nación.
#' @references

getSistau <- function(annual = TRUE) {
  if (annual == TRUE){df %>%
    mutate(year = format(periodo, "%Y")) %>%
      filter(tipo=="SISTAU")%>%
      group_by(year,tipo)%>%
    summarise(monto = sum(monto))}
  else
  {df %>%
     filter(tipo=="SISTAU")%>%
    group_by(periodo, tipo)%>%
     summarise(monto = sum(monto))}
}


#' @title plotSistau
#' @description Plots SISTAU data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @details
#' @rdname plotSistau
#' @author Juan Ignacio Fulponi
#' @source Ministerio de Obras Públicas y Ministerio de Transporte de la Nación.
#' @references

plotSistau <- function(annual = TRUE) {
  if (annual == TRUE){df %>%
      mutate(year = format(periodo, "%Y")) %>%
      filter(tipo=="SISTAU")%>%
      group_by(year,tipo)%>%
      summarise(monto = sum(monto))%>%
        ggplot(aes(year, monto, fill = tipo, colour=tipo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title="Subsidios y compensaciones al transporte automotor, a precios corrientes",
           y = "Pesos")+
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*20, "M"),
                         breaks = 10^7 * c(100,200,300,400)*2
      )
    }
  else
  {df %>%
      filter(tipo=="SISTAU")%>%
      group_by(periodo, tipo)%>%
      summarise(monto = sum(monto))%>%
      ggplot(aes(periodo, monto, fill = tipo, colour=tipo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title="Subsidios y compensaciones al transporte automotor, a precios corrientes",
           y = "Pesos")+
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*20, "M"),
                         breaks = 10^7 * c(100,200,300,400)*2
      )
    }
}



