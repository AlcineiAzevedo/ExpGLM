#'Ajuste de modelos de regressao multipla
#'
#' @description Esta funcao realiza o ajuste de 12 modelos de regressao multipla considerando 2 variaveis independentes (explicativas).
#' @usage BinomialReg(data,TwoWay=F,Family ="probit")
#' @param data    :Se houver apenas um fator (variavel explicativa) a matriz deve
#'  conter duas colunas. A primeira com os valores quantitativos da variavel
#'   explicativa e a segunda com os valores binarios (0 e 1) da variavel resposta. /cr
#' Se houver dois fatores, a matriz deve conter 3 colunas.Obrigatoriamente, as duas
#' primeiras devem ser com as variaveis explicativas e a terceira com a variavel resposta (dependente).
#' @param TwoWay    :valor logico (TRUE ou FALSE). Coloque TRUE quando houver dois fatores
#'  (variaveis explicativas) e FALSE se houver um unico fator.
#' @param Family    :Identificacao do metodo utilizado: "probit" ou "logit".
#' @return A funcao apresenta o resultado do ajuste de 16 modelos de regressao.
#' @seealso /code{/link{aov}}, /code{/link{lm}}
#' @examples
#' ###################################
#' ############ Ajuste de uma regressao glm
#' data(Dados1)
#' D1=Dados1
#' BinomialReg(data=D1,TwoWay=FALSE,Family="probit")
#' @importFrom graphics lines persp
#' @importFrom stats anova binomial glm predict
#' @export


BinomialReg=function(data,TwoWay=F,Family ="probit"){
  D=data
  Fatorial=TwoWay
  Familia=Family
  ########################################################################################
  modelos=list(
    m1  =Z~	 1 + X ,
    m2  =Z~	 1 + X  + I(X^2),
    m3  =Z~	 1 + Y ,
    m4  =Z~	 1 + Y  + I(Y^2),
    m5  =Z~   1 + X  + Y,
    m6  =Z~	 1 + X  + I(X^2)  + Y,
    m7	=Z~	 1 + X  + Y       + I(Y^2),
    m8	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2),
    m9	=Z~	 1 + X  + Y       + X:Y,
    m10	=Z~	 1 + X  + I(X^2)  + Y  + X:Y,
    m11	=Z~	 1 + X  + Y       + I(Y^2)  + X:Y,
    m12	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y,
    m13	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y,
    m14	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(Y^2):X,
    m15	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X,
    m16	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X + I(X^2):I(Y^2))
  #########################################################################################

  if (Fatorial==T){
    X=D[,1]
    Y=D[,2]
    Z=D[,3]}

  if (Fatorial==F){
    X=D[,1]
    Z=D[,2]}

  npar=c(1,2,1,2,2,3,3,4,3,4,4,5,6,6,7,8)

  print("Estudo de regressao binaria")

  if(Fatorial==T){MM=1:16}
  if(Fatorial==F){MM=1:2}

  R2Alcinei=R2McFaddens=AIC=NULL
  for (m in MM){
    print(paste("Analise do modelo ",m))
    model=glm(modelos[[m]],family = binomial(Familia))
    print(paste ("modelo ajustado ->", c(modelos[[m]])))

    print("Analise de Deviance")
    print(anova(model,test ="Chisq"))

    print("----------------------")
    print("Significancia dos coeficientes")
    print(summary(model))

    AIC=c(AIC,model$aic)
    R2McFaddens = c(R2McFaddens, (model$null.deviance- model$deviance)/ model$null.deviance)
    R2Alcinei=c(R2Alcinei,  mean(round(predict(model,type="response"))==Z))

    print("__________________________________________________________________________________________")
  }


  ###################################################################################
  Resumo=cbind(AIC,R2McFaddens,R2Alcinei)
  rownames(Resumo)=paste("modelo",MM)

  print("Resumo dos avaliadores da qualidade do ajuste")
  print(Resumo)

  print("Melhor modelo de acordo com cada avaliador da qualidade do ajuste")
  Resumo2=Resumo
  Resumo2[,2:3]=100-Resumo[,2:3]
  MelhorModelo=apply(Resumo2,2,function(x) order(x)[1])
  print(MelhorModelo)

  if(Fatorial==F){NewData=data.frame(X=seq(min(X),max(X),l=20))}
  if(Fatorial==T){NewData=data.frame(expand.grid(X=seq(min(X),max(X),l=20),Y=seq(min(Y),max(Y),l=20))) }

  model=glm(modelos[[MelhorModelo[1]]],family = binomial(Familia))
  Predito=cbind(NewData,predict(model,newdata = NewData,type="response"))
  if(Fatorial==F){
    plot(Predito[,1],(Predito[,2]),col=0,xlab = colnames(D)[1],ylab = colnames(D)[2])
    lines(Predito[,1],(Predito[,2]))
  }
  if(Fatorial==T){
    persp(x=seq(min(X),max(X),l=20),y=seq(min(Y),max(Y),l=20),matrix(Predito[,3],ncol=20),col = "green3",xlab = colnames(D)[1],ylab = colnames(D)[2],zlab = colnames(D)[3])
  }
  colnames(Predito)=colnames(D)
  return(Predito)

}
