\name{ontoCAR-package}
\alias{ontoCAR-package}
\alias{ontoCAR}
\docType{package}
\title{
The ontoCAT package provides a simple interface to the Experimental Factor Ontology (EFO) and to any other ontology described in OWL or OBO format.
}
\description{
  The ontoCAT package provides a simple interface to the Experimental
  Factor Ontology (\url{http://www.ebi.ac.uk/efo}) and to any other
  ontology described in OWL or OBO format.
    
  Package can load the ontology from a local file or on the fly from a URL and
  internally create the inferred ontology view. Experimental Factor Ontology (EFO)
  is the default ontology, loaded from:
  \url{http://efo.svn.sourceforge.net/viewvc/efo/trunk/src/efoinowl/InferredEFOOWLview/EFO_inferred.owl}.
  The package's methods allow to parse an ontology, search terms in it, find out
  term parents and children.
  The package is based on the Ontology Common API Tasks Java library
  (\url{http://www.ontocat.org}) as well as various other utilites methods and
  depends on rJava R package.
}
\details{
\tabular{ll}{
Package: \tab ontoCAT\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2010-09-20\cr
License: \tab Apache License\cr
LazyLoad: \tab yes\cr
}

}
\author{
    Natalja Kurbatova <natalja@ebi.ac.uk>
}
\references{
    Adamusiak T, Burdett T, van der Velde K J, Abeygunawardena N, Antonakaki D, Parkinson H and Swertz M: OntoCAT -- a simpler way to access ontology resources. \emph{Available from Nature Precedings} \url{http://dx.doi.org/10.1038/npre.2010.4666.1} (2010)  
    
    Malone J, Holloway E, Adamusiak T, Kapushesky M, Zheng J, Kolesnikov N, Zhukova A, Brazma A, Parkinson H: Modeling Sample Variables with an Experimental Factor Ontology. \emph{Bioinformatics} 2010, \bold{26}(8):1112--1118 
    
    Experimental Factor Ontology \url{http://www.ebi.ac.uk/efo}
    
    Ontology Common API Tasks java library \url{http://www.ontocat.org}
    
    Java sources and javadocs: \url{http://sourceforge.net/projects/ontocat/files/}
}

\keyword{ package }

\seealso{\code{\linkS4class{Ontology}}, \code{\linkS4class{OntologyTerm}}, \code{\link{getOntology}} and \code{\link{getEFO}} }

\examples{
    efo<-getEFO()
    ontology <- getOntology("./ontoCAR/extdata/cell.obo")
}
