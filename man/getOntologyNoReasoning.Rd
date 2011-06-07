\name{getOntologyNoReasoning}
\alias{getOntologyNoReasoning}
\title{Returns an instance of the ontology parser created from OWL or OBO file without reasining}
\description{
    Loads the ontology described in OWL or OBO format from the local file or on the fly by using URL.
}
\usage{
    getOntologyNoReasoning(pathToURI)
}
\arguments{
    \item{pathToURI}{a character string giving the URL or local name of the file to load ontology from}
}
\value{
    Returns an instance of the \code{\linkS4class{Ontology}} class.
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

\examples{
    ontologyFromURL <- getOntologyNoReasoning("http://www.ebi.ac.uk/efo/efo.owl")
    getOntologyRelationNames(ontologyFromURL)
}
