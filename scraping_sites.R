library(XML)
library(Rcurl)

linkSiguientePagina =
  function(doc)
  {
    #if(is.empty(baseURL))
    #
    baseURL = "https://www.computrabajo.com.co/empleos-de-informatica-y-telecom"
    pag <- getNodeSet(doc, "//li[@class = 'siguiente']")
    pags <- getNodeSet(pag[[1]], "a")
    urlpag = xmlGetAttr(pags[[1]],"href")
    
    if(length(urlpag) == 0)
      return(character())
    else
      #getRelativeURL(urlpag[[1]], baseURL)
      return(paste(baseURL,urlpag[[1]],sep=""))
    #
  }


doc <- getURL("https://www.computrabajo.com.co/empleos-de-informatica-y-telecom")
avisos <- htmlParse(doc, asText = TRUE)
lst_titulo = c()
lst_url = c()
lst_empresa = c()
lst_descripcion = c()
lst_fecha = c()

print("Trabajando...")

while(TRUE)
{
  tbls = getNodeSet(avisos, "//div[@class = 'iO']")
  navisos <- length(tbls)
  
  for (naviso in 1:navisos)
  {
    titulo = xmlValue(getNodeSet(tbls[[naviso]], "//a[@itemprop='url']")[[naviso]])
    url = xmlGetAttr(getNodeSet(tbls[[naviso]], "//a[@itemprop='url']")[[naviso]],"href")
    empresa = xmlValue(getNodeSet(tbls[[naviso]], "//span[@itemprop='name']")[[naviso]])
    descripcion = xmlValue(getNodeSet(tbls[[naviso]], "//p[@itemprop='description']")[[naviso]])
    fecha = xmlGetAttr(getNodeSet(tbls[[naviso]], "//meta[@itemprop='datePosted']")[[naviso]],"content")
    lst_titulo <- c(lst_titulo,titulo)
    lst_url <- c(lst_url,url)
    lst_empresa <- c(lst_empresa,empresa)
    lst_descripcion <- c(lst_descripcion,descripcion)
    lst_fecha <- c(lst_fecha,fecha)
    
  }
  
  sigPagina = linkSiguientePagina(avisos)
  
  if(length(sigPagina) == 0)
    break
  
  doc <- getURL(sigPagina)
  avisos <- htmlParse(doc, asText = TRUE)
  #print(sigPagina)
}
print("Trabajo finalizado.")
df_anuncios = data.frame(lst_titulo,lst_url,lst_empresa,lst_descripcion,lst_fecha)
df_anuncios
