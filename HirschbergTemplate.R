#cv4 - PRG
#autor: tereza mikešová

NeedleWunsch <- function(sek1,sek2,match,mismatch,gap){
  m <- 1 + nchar(sek1)
  n <- 1 + nchar(sek2)
  sek1 <- strsplit(sek1,split="")
  sek1 <- sek1[[1]]
  sek2 <- strsplit(sek2,split="")
  sek2 <- sek2[[1]]
  S <- (0:(n-1))*gap
  
  for (i in 2:m){
    s <- S[1]
    c <- S[1]+gap
    S[1] <- c
    
    for (j in 2:n){
      if (sek1[i-1] == sek2[j-1]){
        pom <- match
      }
      else{
        pom <- mismatch}
      c <- max(c(S[j]+gap, c+gap, s+pom))
      
      s <- S[j]
      S[j] <- c
      
    }
    
  }
  return(S)
  
}



Hirschberg_sablona <- function(X,Y,Align,match,mismatch,gap){
  Z <- Align[[1]] #inicializace prvniho radku zarovnani, typ DNAStringSet
  W <- Align[[2]] #inicializace druheho radku zarovnani
  
  if (length(X) == 0)       #delka X je nula
  {
    for (i in 1:length(Y))#podle poctu znaku v Y
    {
      Z <- #prida se mezera
        W <- #prida se znak z Y
    }
    Align <- c(DNAStringSet(Z),DNAStringSet(W))
    print(Align)
  }
  else if #delka Y je nula
  {
    for #podle poctu znaku v X
    {
      Z <- #prida se znak z X
        W <- #prida se mezera
    }
    Align <- c(DNAStringSet(Z),DNAStringSet(W))
    print(Align)
  }
  else if #delka X a Y je jedna
  {
    Z <- #prida se znak z X
      W <- #prida se znak z Y
      Align <- c(DNAStringSet(Z),DNAStringSet(W))
    print(Align)
  }
  else
  {
    xlen <- #delka X
      xmid <- #pulka delky X
      ylen <- #delka Y
      
      ScoreL <- # NW skore pro prvni polovinu X a cele Y
      ScoreR <- # NW skore pro druhou polovinu X a cele Y (obe sek jsou obracene)
      ymid <- #index deleni Y
      
      #pro prvni cast
      if #index deleni Y je nula
    {
      Align <- #volani Hirschberg pro prvni polovinu X a prazdny vektor Y
    }
    else
    {
      Align <- #volani Hirschberg pro prvni polovinu X a prvni cast Y
    }
    
    #pro druhou cast
    if ((xmid+1)>xlen) #X jiz nelze pulit
    {
      Align <- #volani Hirschberg pro prazdny vektor X a druhou cast Y
    }
    else if ((ymid+1)>ylen) #Y jiz nejde delit
    {
      Align <- #volani Hirschberg pro druhou polovinu X a prazdny vektor Y
    }
    else 
    {
      Align <- #volani Hirschberg pro druhou polovinu X a druhou cast Y
    }
  }
  return(Align)
}