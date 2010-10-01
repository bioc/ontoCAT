\name{OntologyTerm-class}
\docType{class}
\alias{OntologyTerm-class}
\alias{getAccession,OntologyTerm-method}
\alias{getLabel,OntologyTerm-method}
\alias{show,OntologyTerm-method}

\title{Class "OntologyTerm" }
\description{ External view for an ontological terms in ontoCAT package}
\section{Objects from the Class}{
    Don't create objects of this class. It is a wrapper around an internal Java representation.
}
\section{Slots}{
    \describe{
        \item{\code{term}:}{Object of class \code{"jobjRef"} No user-serviceable parts inside. }
    }
}
\section{Methods}{
    \describe{
        \item{getAccession}{\code{signature(object = "OntologyTerm")}: Returns accession of the term }
        \item{getLabel}{\code{signature(object = "OntologyTerm")}: Returns description of the term }
        \item{show}{\code{signature(object = "OntologyTerm")}: Displays term accession and description string }
    }
}
\references{
    Adamusiak T, Burdett T, van der Velde K J, Abeygunawardena N, Antonakaki D, Parkinson H and Swertz M: OntoCAT -- a simpler way to access ontology resources. \emph{Available from Nature Precedings} \url{http://dx.doi.org/10.1038/npre.2010.4666.1} (2010)  
    
    Malone J, Holloway E, Adamusiak T, Kapushesky M, Zheng J, Kolesnikov N, Zhukova A, Brazma A, Parkinson H: Modeling Sample Variables with an Experimental Factor Ontology. \emph{Bioinformatics} 2010, \bold{26}(8):1112--1118 
    
    Experimental Factor Ontology \url{http://www.ebi.ac.uk/efo}
    
    Ontology Common API Tasks java library \url{http://www.ontocat.org}
    
    Java sources and javadocs: \url{http://sourceforge.net/projects/ontocat/files/}
}
\author{ Tomasz Adamusiak }

\seealso{
    \code{\linkS4class{OntologyParser}}
}

\examples{
    ontologyParser <- getEFOParser()
    term <- getTermById(ontologyParser,"EFO_0001221")
    show(term)
    getAccession(term)
    getLabel(term)
}

\keyword{classes}
