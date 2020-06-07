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
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*300, "M"),
                         breaks = 10^8 * c(100,200,300,400)*3
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
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*300, "M"),
                         breaks = 10^8 * c(100,200,300,400)*3
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
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*300, "M"),
                         breaks = 10^8 * c(100,200,300,400)*3
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
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*300, "M"),
                         breaks = 10^8 * c(100,200,300,400)*3
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
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*300, "M"),
                         breaks = 10^8 * c(100,200,300,400)*3
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
      scale_y_continuous(labels = paste0("$", c(100,200,300,400)*300, "M"),
                         breaks = 10^8 * c(100,200,300,400)*3
      )
  }
}

#' @title getsubsidiosycomp
#' @description Get all subsidies data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @details
#' @rdname getsubsidiosycomp
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
#' @rdname plotsubsidiosycomp
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


#' @title getPax
#' @description Get all available pax data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @return
#' @param type Choose the transport mode. It can be "COLECTIVO", "TREN", "SUBTE" o "TODOS"
#' @details
#' @rdname getPax
#' @author Juan Ignacio Fulponi
#' @source CNRT.
#' @references

getPax <- function(annual = TRUE, type = "TODOS") {
  if (annual == TRUE && type == 'COLECTIVO') {cancelaciones_mes_mmodo %>%
      mutate(year = format(as.Date(periodo), "%Y")) %>%
      filter(modo == 'COLECTIVO')%>%
      group_by(year,modo)%>%
      summarise(pax = sum(pax))

  } else if (annual == TRUE && type == 'TREN') {cancelaciones_mes_mmodo %>%
      mutate(year = format(as.Date(periodo), "%Y")) %>%
      filter(modo == 'TREN')%>%
      group_by(year,modo)%>%
      summarise(pax = sum(pax))

  } else if (annual == TRUE && type == 'SUBTE') {cancelaciones_mes_mmodo %>%
      mutate(year = format(as.Date(periodo), "%Y")) %>%
      filter(modo == 'SUBTE')%>%
      group_by(year,modo)%>%
      summarise(pax = sum(pax))

  } else if (annual == TRUE && type == 'TODOS') {cancelaciones_mes_mmodo %>%
      mutate(year = format(as.Date(periodo), "%Y")) %>%
      group_by(year,modo)%>%
      summarise(pax = sum(pax))

  }else if (annual == FALSE && type == 'COLECTIVO') {cancelaciones_mes_mmodo %>%
      filter(modo == 'COLECTIVO')%>%
      group_by(periodo,modo)%>%
      summarise(pax = sum(pax))

  } else if (annual == FALSE && type == 'TREN') {cancelaciones_mes_mmodo %>%
      filter(modo == 'TREN')%>%
      group_by(periodo,modo)%>%
      summarise(pax = sum(pax))

  } else if (annual == FALSE && type == 'SUBTE') {cancelaciones_mes_mmodo %>%
      filter(modo == 'SUBTE')%>%
      group_by(periodo,modo)%>%
      summarise(pax = sum(pax))
  } else {cancelaciones_mes_mmodo}
}

#' @title plotPax
#' @description Plots all available pax data by period.
#' @param annual `TRUE` or `FALSE`. If `TRUE`, returns annual data. If `FALSE` returns monthly data., Default: `TRUE`.
#' @param type Choose the transport mode. It can be "COLECTIVO", "TREN", "SUBTE" o "TODOS".
#' @return
#' @details
#' @rdname plotPax
#' @author Juan Ignacio Fulponi
#' @source Ministerio de Obras Públicas y Ministerio de Transporte de la Nación.
#' @references

plotPax <- function(annual = TRUE, type = 'TODOS'){
  if (annual == TRUE && type == 'COLECTIVO') {cancelaciones_mes_mmodo %>%
      mutate(year = format(as.Date(periodo), "%Y")) %>%
      filter(modo == 'COLECTIVO')%>%
      group_by(year,modo)%>%
      summarise(pax = sum(pax))%>%
      ggplot(aes(year, pax, fill = modo, colour=modo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title = "Pasajeros pagos de colectivos, en el sistema SUBE.", y = "Pasajeros")+
      scale_y_continuous(labels = paste0(c(100,200,300,400)*10, "M"),
                         breaks = 10^7 * c(100,200,300,400))

    } else if (annual == TRUE && type == 'TREN') {cancelaciones_mes_mmodo %>%
        mutate(year = format(as.Date(periodo), "%Y")) %>%
      filter(modo == 'TREN')%>%
        group_by(year,modo)%>%
      summarise(pax = sum(pax))%>%
      ggplot(aes(year, pax, fill = modo, colour=modo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title = "Pasajeros pagos de trenes, en el sistema SUBE.", y = "Pasajeros")+
        scale_y_continuous(labels = paste0(c(100,200,300,400), "M"),
                           breaks = 10^6 * c(100,200,300,400))

      } else if (annual == TRUE && type == 'SUBTE') {cancelaciones_mes_mmodo %>%
          mutate(year = format(as.Date(periodo), "%Y")) %>%
      filter(modo == 'SUBTE')%>%
          group_by(year,modo)%>%
      summarise(pax = sum(pax))%>%
      ggplot(aes(year, pax, fill = modo, colour=modo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title = "Pasajeros pagos de subtes, en el sistema SUBE.", y = "Pasajeros")+
          scale_y_continuous(labels = paste0(c(100,200,300,400), "M"),
                             breaks = 10^6 * c(100,200,300,400))

        } else if (annual == TRUE && type == 'TODOS') {cancelaciones_mes_mmodo %>%
      mutate(year = format(as.Date(periodo), "%Y")) %>%
            group_by(year,modo)%>%
      summarise(pax = sum(pax))%>%
      ggplot(aes(year, pax, fill = modo, colour=modo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title = "Pasajeros pagos en el sistema SUBE.", y = "Pasajeros")+
            scale_y_continuous(labels = paste0(c(100,200,300,400), "M"),
                               breaks = 10^6 * c(100,200,300,400))

          }else if (annual == FALSE && type == 'COLECTIVO') {cancelaciones_mes_mmodo %>%
      filter(modo == 'COLECTIVO')%>%
              group_by(periodo, modo)%>%
      summarise(pax = sum(pax))%>%
      ggplot(aes(periodo, pax, fill = modo, colour=modo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title = "Pasajeros pagos de colectivos, en el sistema SUBE.", y = "Pasajeros")+
              scale_y_continuous(labels = paste0(c(100,200,300,400), "M"),
                                 breaks = 10^6 * c(100,200,300,400))

            } else if (annual == FALSE && type == 'TREN') {cancelaciones_mes_mmodo %>%
      filter(modo == 'TREN')%>%
                group_by(periodo, modo)%>%
                summarise(pax = sum(pax))%>%
                ggplot(aes(periodo, pax, fill = modo, colour=modo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title = "Pasajeros pagos de trenes, en el sistema SUBE.", y = "Pasajeros")+
                scale_y_continuous(labels = paste0(c(100,200,300,400), "M"),
                                   breaks = 10^6 * c(100,200,300,400))

              } else if (annual == FALSE && type == 'SUBTE') {cancelaciones_mes_mmodo %>%
      filter(modo == 'SUBTE')%>%
                  group_by(periodo, modo)%>%
                  summarise(pax = sum(pax))%>%
                  ggplot(aes(periodo, pax, fill = modo, colour=modo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title = "Pasajeros pagos de subtes, en el sistema SUBE.", y = "Pasajeros")+
                  scale_y_continuous(labels = paste0(c(100,200,300,400), "M"),
                                     breaks = 10^6 * c(100,200,300,400))

                } else {cancelaciones_mes_mmodo %>%
      ggplot(aes(periodo, pax, fill = modo, colour=modo))+
      geom_col()+
      theme(axis.title.x = element_text(color="white"))+
      labs(title = "Pasajeros pagos en el sistema SUBE.", y = "Pasajeros")+
                    scale_y_continuous(labels = paste0(c(100,200,300,400)*10, "M"),
                                       breaks = 10^7 * c(100,200,300,400)
                    )}
}
