\name{OntologyParser-class}
\docType{class}
\alias{OntologyParser-class}
\alias{getAllTermChildren,OntologyParser,character-method}
\alias{getAllTermIds,OntologyParser-method}
\alias{getAllTermParents,OntologyParser,character-method}
\alias{getAllTerms,OntologyParser-method}
\alias{getDefinitions,OntologyParser,character-method}
\alias{getEFOBranchRootIds,OntologyParser-method}
\alias{getOntologyAccession,OntologyParser-method}
\alias{getOntologyDescription,OntologyParser-method}
\alias{getRootIds,OntologyParser-method}
\alias{getRoots,OntologyParser-method}
\alias{getSynonyms,OntologyParser,character-method}
\alias{getTermAndAllChildrenIds,OntologyParser,character-method}
\alias{getTermById,OntologyParser,character-method}
\alias{getTermChildren,OntologyParser,character-method}
\alias{getTermNameById,OntologyParser,character-method}
\alias{getTermParents,OntologyParser,character-method}
\alias{getTreeDownTo,OntologyParser,character-method}
\alias{hasTerm,OntologyParser,character-method}
\alias{isEFOBranchRoot,OntologyParser,character-method}
\alias{isRoot,OntologyParser,character-method}
\alias{searchTerm,OntologyParser,character-method}
\alias{searchTermPrefix,OntologyParser,character-method}

\title{Class "OntologyParser" }
\description{Parsing and representation of ontology hierarchy}
\section{Accessing the ontologies}{
    The appropriate way to access ontology is via the helper \code{\link{getOntologyParser}} function.
}
\section{Accessing the EFO}{
    The appropriate way to access EFO is via the helper \code{\link{getEFOParser}} function.
}
\section{Slots}{
    \describe{
        \item{\code{ontologyParser}:}{Object of class \code{"jobjRef"} No user-serviceable parts inside. Maps to an internal Java OntologyParser object. }
    }
}
\section{Methods}{
    \describe{
        \item{getAllTermChildren}{\code{signature(object = "OntologyParser", id = "character")}: Returns list of term's all children }
        \item{getAllTermIds}{\code{signature(object = "OntologyParser")}: Returns list of all term accessions }
        \item{getTermAllParents}{\code{signature(object = "OntologyParser", id = "character")}: Returns list of term's all parents }
        \item{getAllTerms}{\code{signature(object = "OntologyParser")}: Returns list of all terms }
        \item{getDefinitions}{\code{signature(object = "OntologyParser", id = "character")}: Returns set of term's definitions if there are some }
        \item{getEFOBranchRootIds}{\code{signature(object = "OntologyParser")}: Returns set of branch root accessions. Method specific for EFO ontology }
        \item{getOntologyAccession}{\code{signature(object = "OntologyParser")}: Returns parsed ontology accession }
        \item{getOntologyDescription}{\code{signature(object = "OntologyParser")}: Returns parsed ontology description }
        \item{getRootIds}{\code{signature(object = "OntologyParser")}: Returns list of root terms accessions, if there are any }
        \item{getRoots}{\code{signature(object = "OntologyParser")}: Returns list of root terms, if there are any }
        \item{getSynonyms}{\code{signature(object = "OntologyParser", id = "character")}: Returns set of term's synonyms  if there are some }
        \item{getTermAndAllChildrenIds}{\code{signature(object = "OntologyParser", id = "character")}: Returns list of accessions of term itself and all its children recursively }
        \item{getTermById}{\code{signature(object = "OntologyParser", id = "character")}:  Fetch term by accession. Returns external term representation if found in ontology, null otherwise }
        \item{getTermChildren}{\code{signature(object = "OntologyParser", id = "character")}: Returns list of term's direct children }
        \item{getTermNameById}{\code{signature(object = "OntologyParser", id = "character")}: Returns term's label by accession }
        \item{getTermParents}{\code{signature(object = "OntologyParser", id = "character")}:  Returns list of term's direct parents }
        \item{getTreeDownTo}{\code{signature(object = "OntologyParser", id = "character")}: Returns set of terms that represent ontology "opened" down to specified term, hence displaying all its parents first and then a tree level, containing specified term }
        \item{hasTerm}{\code{signature(object = "OntologyParser", id = "character")}: Check if term with specified accession exists in ontology }
        \item{isEFOBranchRoot}{\code{signature(object = "OntologyParser", id = "character")}: Returns true if term is branch root of EFO. Method specific for EFO ontology }
        \item{isRoot}{\code{signature(object = "OntologyParser", id = "character")}: Returns true if term is root of ontology}
        \item{searchTerm}{\code{signature(object = "OntologyParser", id = "character")}: Searches for term in ontology by name }
        \item{searchTermPrefix}{\code{signature(object = "OntologyParser", prefix = "character")}: Searches for prefix in ontology }
    }
}
\references{
    Adamusiak T, Burdett T, van der Velde K J, Abeygunawardena N, Antonakaki D, Parkinson H and Swertz M: OntoCAT -- a simpler way to access ontology resources. \emph{Available from Nature Precedings} \url{http://dx.doi.org/10.1038/npre.2010.4666.1} (2010)  
    
    Malone J, Holloway E, Adamusiak T, Kapushesky M, Zheng J, Kolesnikov N, Zhukova A, Brazma A, Parkinson H: Modeling Sample Variables with an Experimental Factor Ontology. \emph{Bioinformatics} 2010, \bold{26}(8):1112--1118 
    
    Experimental Factor Ontology \url{http://www.ebi.ac.uk/efo}
    
    Ontology Common API Tasks java library \url{http://www.ontocat.org}
    
    Java sources and javadocs: \url{http://sourceforge.net/projects/ontocat/files/}
}
\author{ Misha Kapushesky, Pavel Kurnosov, Natalja Kurbatova }
\note{This package ships with the EFO OWL file, version released at the time of the package build. Provided EFO OWL file can be loaded as any other OWL or OBO file by using \code{\link{getOntologyParser}} function. 

Another option is to load the latest EFO version on the fly by using \code{\link{getEFOParser}} function.}

\seealso{
    \code{\link{getOntologyParser}},\code{\link{getEFOParser}} and \code{\linkS4class{OntologyTerm}}
}

\examples{
    ontologyParser <- getEFOParser()
    getEFOBranchRootIds(ontologyParser)
    getTermParents(ontologyParser,"EFO_0001221")
    searchTermPrefix(ontologyParser,"leuk")
    getTermAndAllChildrenIds(ontologyParser,"EFO_0000318")
    searchTerm(ontologyParser,"thymus")
    ontologyParser <- getOntologyParser("http://efo.svn.sourceforge.net/viewvc/efo/trunk/src/efoinowl/InferredEFOOWLview/EFO_inferred.owl")
    ontologyParser <- getOntologyParser("./ontoCAT/extdata/cell.obo")
    getAllTermIds(ontologyParser)
    
}

\keyword{classes}
