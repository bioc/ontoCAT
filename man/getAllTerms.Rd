\name{getAllTerms}
\alias{getAllTerms}
\title{Returns all ontology terms }
\description{
    Returns set of ontology terms, where each term is an instance of the \code{\linkS4class{OntologyTerm}} class.  
}
\usage{
    getAllTerms(object)
}
\arguments{
    \item{object}{instance of the \code{\linkS4class{OntologyParser}} class}
}
\value{
    Returns all terms from loaded ontology as objects of \code{\linkS4class{OntologyTerm}} class.
}
\references{
    Adamusiak T, Burdett T, van der Velde K J, Abeygunawardena N, Antonakaki D, Parkinson H and Swertz M: OntoCAT -- a simpler way to access ontology resources. \emph{Available from Nature Precedings} \url{http://dx.doi.org/10.1038/npre.2010.4666.1} (2010)  
    
    Malone J, Holloway E, Adamusiak T, Kapushesky M, Zheng J, Kolesnikov N, Zhukova A, Brazma A, Parkinson H: Modeling Sample Variables with an Experimental Factor Ontology. \emph{Bioinformatics} 2010, \bold{26}(8):1112--1118 
    
    Experimental Factor Ontology \url{http://www.ebi.ac.uk/efo}
    
    Ontology Common API Tasks java library \url{http://www.ontocat.org}
    
    Java sources and javadocs: \url{http://sourceforge.net/projects/ontocat/files/}
}
\author{Misha Kapushesky, Pavel Kurnosov, Natalja Kurbatova}
    \seealso{\code{\linkS4class{OntologyParser}} and \code{\linkS4class{OntologyTerm}}}
\examples{
    efo <- getEFOParser()
    getAllTerms(efo)
}