\name{getTermChildren}
\alias{getTermChildren}
\title{Returns direct children of term of interest}
\description{
    Returns set of direct children of the term of interest. Term in the set is represented as the instance of the \code{\linkS4class{OntologyTerm}} class
}
\usage{
    getTermChildren(object,id)
}
\arguments{
    \item{object}{instance of the \code{\linkS4class{OntologyParser}} class}
    \item{id}{accession string of the term of interest}
}
\value{
    Returns set of ontology terms: each term in the set is the instance of the \code{\linkS4class{OntologyTerm}} class
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
    getTermChildren(efo,"EFO_0000322")
}