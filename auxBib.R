
if(!require(stringr)){install.packages("stringr");require(stringr)}

# Função que retorna 1 para sim e 0 para não possui função lapply é
# para aplicar na lista função str_count é contar quantas vezes certa
# 'string' aparece em um vetor função str_replace_all é para
# subistituir(remover) os parenteses que por algum motivo causavam erro
# na função str_count
sim_ou_nao <- function(separacao, nome) {
  unlist(lapply(separacao, function(x) {
    sum(str_count(str_replace_all(x, "[(,)]", ""), paste0("^",str_replace_all(nome, "[(,)]", ""),"$") ))
  }))
}


# Função que retorna a matriz binária para a Variável j isoladamente
Gerar_matriz_binaria <- function(x, j, dados) {
  separacao <- strsplit(as.character(x), ",")     #separa as respostas de cada um pela vírgula, gera uma lista p/ cada respondente
  separacao <- lapply(separacao, function(x) {    # Faz uma correção em uma questão específica que continha um item com ","
    x[which(x == "Treinamentos de usuários (normalização" | 
              x == " Treinamentos de usuários (normalização")] <- c("Treinamentos de usuários (normalização, bases de dados etc.)")
    x[which(x == "bases de dados etc.)" | x == " bases de dados etc.)")] <- NA
    x <- na.exclude(x)
  })
  separacao <- lapply(separacao, function(x) str_trim(x, side = "left"))    # tira todos os espaços do início do vetor
  separacao <- lapply(separacao, function(x){if(length(x)==0) x<-NA else x<-x})   # transforma em NA os campos vazios
  
  
  nlevel <- length(levels(as.factor(unlist(separacao))))    # número de itens
  nameslevels <- levels(as.factor(unlist(separacao)))       # nomes dos itens
  
  # verificar se é uma questão multitem, 1 se sim e 0 se não
  if( sum(unlist(lapply(separacao, length))) > length(x) ) multitem <- 1 else multitem <- 0
  
  if(multitem == 0){  # caso não seja multitem ele retorna os próprios dados
    matriz <- as.data.frame(unlist(separacao)[])
    colnames(matriz) <- dimnames(dados)[[2]][j]
  }else{    # caso seja multitem ele retorna a matriz de 0 e 1
    matriz <- sim_ou_nao(separacao, nameslevels[1])
    for (i in 2:nlevel) {
      matriz <- cbind(matriz, sim_ou_nao(separacao, nameslevels[i]))
    }
#    colnames(matriz) <- paste0("V", j, ": ", nameslevels)
    colnames(matriz) <- paste0(nameslevels)
  }
  
  # retorna a matriz com os dados e as variáveis que são multitem
  return(list( Matriz = matriz, Multitem = multitem, N = nlevel))
}