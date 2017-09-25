library(XML)
library(RCurl)

rm(list = ls())

linkSiguientePagina =
  function(doc)
  {
    #if(is.empty(baseURL))
    #
    baseURL <- "http://www.elempleo.com/co/ofertas-empleo/?icid=prefoo_hom_empleos"
    pag <- getNodeSet(doc, "//a[@class = 'js-btn-next']")
    if (length(pag) == 0)
      return(character())
    
    pags <- getNodeSet(pag[[1]], "a")
    urlpag = xmlGetAttr(pags[[1]],"href")
    
    if(length(urlpag) == 0)
      return(character())
    else
      #getRelativeURL(urlpag[[1]], baseURL)
      return(paste(baseURL,urlpag[[1]],sep=""))
    #
  }

doc     <- getURL("http://www.elempleo.com/co/ofertas-empleo/?icid=prefoo_hom_empleos")
avisos  <- htmlParse(doc, asText = TRUE)
lst_titulo <- c()
lst_url <- c()
lst_empresa <- c()
lst_salario <- c()
lst_fecha <- c()

print("Trabajando...")

while(TRUE)
{
  tbls <- getNodeSet(avisos, "//ul[@class = 'list-unstyled result-info']")
  navisos <- length(tbls)
  
  for (naviso in 1:navisos)
  {
    titulo  <- xmlGetAttr(getNodeSet(tbls[[naviso]],"//h2[@itemprop ='title']//a[@class = 'text-ellipsis']")[[naviso]],"title")
    url     <- xmlGetAttr(getNodeSet(tbls[[naviso]],"//h2[@itemprop ='title']//a[@class = 'text-ellipsis']")[[naviso]],"href")
    empresa <- gsub('\r\n',"",xmlValue(getNodeSet(tbls[[naviso]],"//span[@itemprop ='name']")[[naviso]]))
    salario <- gsub("\r\n","",xmlValue(getNodeSet(tbls[[naviso]],"//span[@itemprop ='baseSalary']")[[naviso]]))
    fecha   <- gsub("\r\n","",xmlValue(getNodeSet(tbls[[naviso]],"//span[@class ='info-publish-date pull-right']")[[naviso]]))

    lst_titulo      <- c(lst_titulo,titulo)
    lst_url         <- c(lst_url,url)
    lst_empresa     <- c(lst_empresa,empresa)
    lst_salario     <- c(lst_salario,salario)
    lst_fecha       <- c(lst_fecha,fecha)
    
  } 
  
  # // Btn go to next page
  # $container.on('click', '.js-btn-next', function(event){
  #   var page;
  #   event.preventDefault();
  #   
  #   page = context.pageIndex;
  #   if(page !== context.totalPages) {
  #     page++;
  #     callback('pageNumb', page);
  #   }
  # });
  
  
  sigPagina <- linkSiguientePagina(avisos)
  
  if(length(sigPagina) == 0){
    break
  }
  
  doc <- getURL(sigPagina)
  avisos <- htmlParse(doc, asText = TRUE)
  
}
print("Trabajo finalizado.")
df_anuncios = data.frame(lst_titulo,lst_url,lst_empresa,lst_salario,lst_fecha)
#df_anuncios
