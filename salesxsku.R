SKU <- unique(salesxsku$sku)
clientemes <- 5250
dateini <- "2021-02-16"

trendsku<- sapply(SKU,function(SKU){
  
  datemax <- salesxsku %>% filter(date >= dateini & date < "2021-02-28" & sku == SKU)%>%
    slice_head(n = 200)%>% group_by (sku) %>%
    mutate(promedio = mean(qty),maximo = max(qty), clientes = n_distinct(id), total = sum(qty),date) %>%
    filter(maximo <= 10 | qty != maximo)%>% summarise(date)%>% slice_tail()%>% pull(date)
  
  clientemesp <- salesxsku %>% filter(date >= dateini &  date <= datemax) %>% summarize(clientes = n_distinct(id))%>% pull(clientes)
  
  print.data.frame(salesxsku %>% filter(date >= dateini & date <= datemax & sku == SKU) %>% group_by (sku) %>%
    mutate(promedio = mean(qty),maximo = max(qty), clientes = n_distinct(id), totalunits = sum(qty)) %>%
    filter(maximo <= 10 | qty != maximo) %>%
      summarise(promedio = mean(qty),maximo = max(qty),clientes =n_distinct(id),totalunits = sum(qty), clientF = clientes/clientemesp,trend = clientes/clientemesp*clientemes*promedio), clienteperiodo = clientemesp/1)
}
)`
trendskufinal <-t(trendsku)
write.csv(trendskufinal,"D:/SALES/trendsku02182021.csv"