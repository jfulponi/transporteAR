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

#' @title getRCC
#' @description Get RCC (Régimen de Compensaciones complementarias para AMBA) data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @details
#' @rdname getRCC
#' @author Juan Ignacio Fulponi
#' @source Ministerio de Obras Públicas y Ministerio de Transporte de la Nación.
#' @references

getRCC <- function(annual = TRUE) {
  if (annual == TRUE){df %>%
      mutate(year = format(periodo, "%Y")) %>%
      filter(tipo=="RCC")%>%
      group_by(year,tipo)%>%
      summarise(monto = sum(monto))}
  else
  {df %>%
      filter(tipo=="RCC")%>%
      group_by(periodo, tipo)%>%
      summarise(monto = sum(monto))}
}


#' @title plotRCC
#' @description Plots RCC data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @details
#' @rdname plotRCC
#' @author Juan Ignacio Fulponi
#' @source Ministerio de Obras Públicas y Ministerio de Transporte de la Nación.
#' @references

plotRCC <- function(annual = TRUE) {
  if (annual == TRUE){df %>%
      mutate(year = format(periodo, "%Y")) %>%
      filter(tipo=="RCC")%>%
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
      filter(tipo=="RCC")%>%
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

#' @title getCCP
#' @description Get CCP (Compensaciones complementarias para las provincias) data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @details
#' @rdname getCCP
#' @author Juan Ignacio Fulponi
#' @source Ministerio de Obras Públicas y Ministerio de Transporte de la Nación.
#' @references

getCCP <- function(annual = TRUE) {
  if (annual == TRUE){df %>%
      mutate(year = format(periodo, "%Y")) %>%
      filter(tipo=="CCP")%>%
      group_by(year,tipo)%>%
      summarise(monto = sum(monto))}
  else
  {df %>%
      filter(tipo=="CCP")%>%
      group_by(periodo, tipo)%>%
      summarise(monto = sum(monto))}
}


#' @title plotCCP
#' @description Plots CCP data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @details
#' @rdname plotCCP
#' @author Juan Ignacio Fulponi
#' @source Ministerio de Obras Públicas y Ministerio de Transporte de la Nación.
#' @references

plotCCP <- function(annual = TRUE) {
  if (annual == TRUE){df %>%
      mutate(year = format(periodo, "%Y")) %>%
      filter(tipo=="CCP")%>%
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
      filter(tipo=="CCP")%>%
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

#' @title getsubsidiosycomp
#' @description Get all subsidies data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @details
#' @rdname getCCP
#' @author Juan Ignacio Fulponi
#' @source Ministerio de Obras Públicas y Ministerio de Transporte de la Nación.
#' @references

getsubsidiosycomp <- function(annual = TRUE) {
  if (annual == TRUE){df %>%
      mutate(year = format(periodo, "%Y")) %>%
      filter(tipo %in% c("CCP", "RCC", "SISTAU"))%>%
      group_by(year,tipo)%>%
      summarise(monto = sum(monto))}
  else
  {df %>%
      filter(tipo %in% c("CCP", "RCC", "SISTAU"))%>%
      group_by(periodo, tipo)%>%
      summarise(monto = sum(monto))}
}


#' @title plotsubsidiosycomp
#' @description Plots all subsidies data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @details
#' @rdname plotCCP
#' @author Juan Ignacio Fulponi
#' @source Ministerio de Obras Públicas y Ministerio de Transporte de la Nación.
#' @references

plotsubsidiosycomp <- function(annual = TRUE) {
  if (annual == TRUE){df %>%
      mutate(year = format(periodo, "%Y")) %>%
      filter(tipo %in% c("CCP", "RCC", "SISTAU"))%>%
      group_by(year,tipo)%>%
      summarise(monto = sum(monto))%>%
      ggplot(aes(year, monto, fill = tipo, colour=tipo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title="Subsidios y compensaciones al transporte automotor, a precios corrientes",
           y = "Pesos")+
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*300, "M"),
                         breaks = 10^8 * c(100,200,300,400)*3
      )
  }
  else
  {df %>%
      filter(tipo %in% c("CCP", "RCC", "SISTAU"))%>%
      group_by(periodo, tipo)%>%
      summarise(monto = sum(monto))%>%
      ggplot(aes(periodo, monto, fill = tipo, colour=tipo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title="Subsidios y compensaciones al transporte automotor, a precios corrientes",
           y = "Pesos")+
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*300, "M"),
                         breaks = 10^8 * c(100,200,300,400)*3
      )
  }
}

