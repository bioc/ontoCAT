\name{getTermRelations}
\alias{getTermRelations}
\title{Returns set of terms that are in defined relation with term of interest}
\description{
    Returns set of terms that are in defined relation with the term of interest
}
\usage{
    getTermRelations(object1,object2,relation)
}
\arguments{
    \item{object1}{instance of the \code{\linkS4class{Ontology}} class}
    \item{object2}{instance of the \code{\linkS4class{OntologyTerm}} class}
    \item{relation}{relation name}
}
\value{
    Returns set of ontology term's synonymss if there are some
}
\references{
    Adamusiak T, Burdett T, van der Velde K J, Abeygunawardena N, Antonakaki D, Parkinson H and Swertz M: OntoCAT -- a simpler way to access ontology resources. \emph{Available from Nature Precedings} \url{http://dx.doi.org/10.1038/npre.2010.4666.1} (2010)  
    
    Malone J, Holloway E, Adamusiak T, Kapushesky M, Zheng J, Kolesnikov N, Zhukova A, Brazma A, Parkinson H: Modeling Sample Variables with an Experimental Factor Ontology. \emph{Bioinformatics} 2010, \bold{26}(8):1112--1118 
    
    Experimental Factor Ontology \url{http://www.ebi.ac.uk/efo}
    
    Ontology Common API Tasks java library \url{http://www.ontocat.org}
    
    Java sources and javadocs: \url{http://sourceforge.net/projects/ontocat/files/}
}
\author{Natalja Kurbatova}
    \seealso{\code{\linkS4class{Ontology}} and \code{\linkS4class{OntologyTerm}}}