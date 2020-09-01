
rm(list = ls())

porcentaje <- function(x, decimales = 2){
    x <- as.data.frame(x)
    x$Porcentaje <- paste0(round(x$Porcentaje, decimales), "%")
    x
} 

tabla <- function(x){
    x %>% porcentaje() %>%  kbl() %>% kable_paper("hover", full_width = TRUE)
}


tabla_d <- function(x){
    DT::datatable(x, extensions =  'Buttons', options =  list(dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'print')))
}


# elecciones presidenciales
elec <- elecciones_uy %>% 
    filter(eleccion == "Presidencial") %>% 
    select(anio_eleccion) %>% 
    unique %>% 
    deframe()

for(i in 1:length(elec)) { 
    bor <- paste0("e_", elec[i])
    assign(bor, nacional_uy(eleccion = elec[i]))
}
e_1930$Diputados <- 0
e_1930$Senadores <- 0
ELEC <- grep(x = ls(), pattern = "^e_", value = TRUE)
data_pres <- do.call('rbind', mget(ELEC, ifnotfound = as.list(ELEC))) %>% 
    arrange(Fecha, Votos) %>% transform('Porcentaje' = as.character(round(.$Porcentaje, 2)))


# elecciones departamentales
elecd <- elecciones_uy %>% 
    filter(eleccion == "Departamental") %>% 
    select(anio_eleccion) %>% 
    unique %>% 
    deframe()

for(i in 1:length(elecd)) { 
    bor <- paste0("e_",elec[i], "d")
    assign(bor, nacional_uy(eleccion = elec[i], por_departamento = TRUE))
}

meses <- function(m){
    if(m == "10") return("octubre")
    if(m == "11") return("noviembre")
}

# graficos departamentos
grafico_departamentos <- function(datos){
    
    anio  <- substring(datos$Fecha[1], 1, 4)
    fecha <- paste(substring(datos$Fecha[1], 9, 10), 'de', meses(substring(datos$Fecha[1], 6, 7)), "de", anio)
        
    ggplot(data = datos , aes(x = reorder(Sigla, Porcentaje), y = Porcentaje)) +
        geom_bar(stat="identity", position = "stack", fill ="#009999", color = "black") +
        ylim(0,100) +
        coord_flip() +
        geom_text(aes(label = paste0(Porcentaje, "%")), hjust = -0.5, color = "gray9", size = 2) +
        labs(x = "", y = "",
             title = paste("Resultado de elecci\u00f3n nacional de", anio),
             subtitle = fecha) +
        theme_minimal() + 
        theme(axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              legend.position = "none") +
        facet_wrap(~Departamento)
}




