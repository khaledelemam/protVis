#human p53
seq1 <- "MEEPQSDPSVEPPLSQETFSDLWKLLPENNVLSPLPSQAMDDLMLSPDDIEQWFTEDPGPDEAPRMPEAA
PPVAPAPAAPTPAAPAPAPSWPLSSSVPSQKTYQGSYGFRLGFLHSGTAKSVTCTYSPALNKMFCQLAKT
CPVQLWVDSTPPPGTRVRAMAIYKQSQHMTEVVRRCPHHERCSDSDGLAPPQHLIRVEGNLRVEYLDDRN
TFRHSVVVPYEPPEVGSDCTTIHYNYMCNSSCMGGMNRRPILTIITLEDSSGNLLGRNSFEVRVCACPGR
DRRTEEENLRKKGEPHHELPPGSTKRALPNNTSSSPQPKKKPLDGEYFTLQIRGRERFEMFRELNEALEL
KDAQAGKEPGGSRAHSSHLKSKKGQSTSRHKKLMFKTEGPDSD
"

#mouse p53
seq2 <- "MEEPQSDLSIEAPLSQETFSDLWKLLPENNVLSSSLSPPVDDLMLSAEDFAGWFTEGPDEAARMSENPEP
AAPVPATPTPVASTPTWPLSSSVPSQKTYPGNYGFRLGFLHSGTAKSVTCTYSPALNKMFCQLAKTCPVQ
LWVDSTPPPGSRIRAMAIYKQSQHMTEVVRRCPHHERCSDSDGLAPSQHLIRVERNLRVEYLDDRNTFRH
SVVVPYEPPEVGSDCTTIHYNYMCNSSCMGGMNRRPIVTIITLEDSNGIVLGRNSFEVRVCACPGRDRRT
EEENFRKKGEPCSGSAKRALPTNTSSSPPPKKKPLDGEYFTLQIRGRERFEMFRMLNEALEFKDAQTGKE
PGESRAHSSHLKSKKGQSTSRHKKLMFKREGPDSD"


#' Calculates the differences in amino acid sequence and return a data frame
#'
#' @param x first sequence
#' @param y second sequence
#' @return data frame
#' @examples
#' AADiff(sequence1, sequence2)

AADiff <- function(seq1, seq2){

  if(nchar(seq1)!=nchar(seq2)) {
    val = abs(nchar(seq1)-nchar(seq2))
    vec <- vector()

    for (i in 1:val){
      vec <- c(vec, "*")
    }

    pad <- paste(vec,collapse="")


    if (nchar(seq1)< nchar(seq2)){
      temp <- paste(seq1, pad, sep="")
      seq1 <- temp
    }
    else{
      temp <- paste(seq2, pad, sep="")
      seq2 <- temp
    }
  }

  seq1Diff <- unlist(strsplit(seq1,split=""))
  seq2Diff <- unlist(strsplit(seq2,split=""))

  diff <-rbind(seq1Diff ,seq2Diff)

  onlyDiff<-diff[,diff[1,]!=diff[2,]]

  position<-which(diff[1,]!=diff[2,])

  onlyDiff<-rbind(position,onlyDiff)

  finalFrame <- data.frame(t(onlyDiff))

  return (finalFrame)
}



#' Plot a scatter plot of the sequence difference.
#'
#' @param no param
#'
#' @return scatter plot
#' @examples
#' plotData
plotData <-function(){
  frame <- AADiff(seq1, seq2)


  ggplot(data = frame, aes(x=position, y=seq1Diff)) + geom_point() + scale_x_discrete(breaks = seq(1, 400, by = 50))

  ggplot(data = frame, aes(x=position, y=seq2Diff)) + geom_point() + scale_x_discrete(breaks = seq(1, 400, by = 50))
}
