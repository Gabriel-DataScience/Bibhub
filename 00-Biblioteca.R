
Gerar_Matriz_Ajustada <- function(dados, salvar = TRUE) {
  
  
  # criando uma matriz Geral para todas as vari?veis
  
  j <- 1
#  multitem <- Gerar_matriz_binaria(dados[, 1], j)$Multitem
#  nlevel <- Gerar_matriz_binaria(dados[, 1], j)$N
  matriz <- Gerar_matriz_binaria(dados[, 1], j, dados)$Matriz    # primeiro ? feita a primeira coluna
  lista <- list(matriz)                                   # lista para conter a matriz das questões separadas
  for (i in 2:ncol(dados)) {                # depois as demais colunas, pois uso o cbind para criar a matriz final
#    multitem <- c(multitem, Gerar_matriz_binaria(dados[, i], j)$Multitem)
#    nlevel <- c(nlevel, Gerar_matriz_binaria(dados[, i], j)$N)
    j <- j + 1
    matrizi <- Gerar_matriz_binaria(dados[, i], j, dados)$Matriz
    lista <- c(lista, list(matrizi))
    matriz <- cbind(matriz, matrizi)
    
  }
  
  names(lista) <- colnames(dados)       # nomeando os elementos da lista
  
 # ncolunas_var <- nlevel
 # if(all(multitem == 0)) ncolunas_var <- rep(1, length(nlevel)) else ncolunas_var[which(multitem == 0)] <- 1
  
  # preparando a sa?da em xls
  Resultado <- cbind(matriz, rep("", nrow(dados)), c(paste0("V", 1:ncol(dados)), rep("", (nrow(dados) - ncol(dados)))), 
                     c(colnames(dados), rep("",(nrow(dados) - ncol(dados)))))
  # salvando em xls
  if(salvar == TRUE){
    write.table(Resultado, file = "./Matrix_Ajustada.csv", sep = ";",row.names = FALSE, col.names = TRUE)
  }
  
  # frequ?ncia e percentual
  freq_aux <- function(x){
    if(is.numeric(x)){
      aux <- cbind(colnames(x), colSums(x, na.rm = TRUE),round(colSums(x, na.rm = TRUE)/nrow(dados)*100,1))
      colnames(aux) <- c("Opções","Frequência","%")
    }else{
      aux <- cbind(names(table(x)), table(x), round(table(x)/nrow(dados)*100,1))
      colnames(aux) <- c("Opções","Frequência","%")
    }
    return(aux)
  }
  frequencia <- lapply(lista, freq_aux)
  
  invisible(list(Matriz = matriz,  Lista = lista, Frequencia = frequencia))
}



