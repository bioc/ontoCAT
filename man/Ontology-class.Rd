\name{Ontology-class}
\docType{class}
\alias{Ontology-class}
\alias{getAllTermChildren,Ontology,OntologyTerm-method}
\alias{getAllTermChildrenById,Ontology,character-method}
\alias{getAllTermIds,Ontology-method}
\alias{getAllTermParents,Ontology,OntologyTerm-method}
\alias{getAllTermParentsById,Ontology,character-method}
\alias{getAllTerms,Ontology-method}
\alias{getEFOBranchRootIds,Ontology-method}
\alias{getOntologyAccession,Ontology-method}
\alias{getOntologyDescription,Ontology-method}
\alias{getRootIds,Ontology-method}
\alias{getRoots,Ontology-method}
\alias{getTermAndAllChildren,Ontology,OntologyTerm-method}
\alias{getTermAndAllChildrenById,Ontology,character-method}
\alias{getTermById,Ontology,character-method}
\alias{getTermChildren,Ontology,OntologyTerm-method}
\alias{getTermChildrenById,Ontology,character-method}
\alias{getTermDefinitions,Ontology,OntologyTerm-method}
\alias{getTermDefinitionsById,Ontology,character-method}
\alias{getTermNameById,Ontology,character-method}
\alias{getTermParents,Ontology,OntologyTerm-method}
\alias{getTermParentsById,Ontology,character-method}
\alias{getTermSynonyms,Ontology,OntologyTerm-method}
\alias{getTermSynonymsById,Ontology,character-method}
\alias{hasTerm,Ontology,character-method}
\alias{isEFOBranchRoot,Ontology,OntologyTerm-method}
\alias{isEFOBranchRootById,Ontology,character-method}
\alias{isRoot,Ontology,OntologyTerm-method}
\alias{isRootById,Ontology,character-method}
\alias{searchTerm,Ontology,character-method}
\alias{searchTermPrefix,Ontology,character-method}
\alias{showHierarchyDownToTerm,Ontology,OntologyTerm-method}
\alias{showHierarchyDownToTermById,Ontology,character-method}
\alias{showPathsToTerm,Ontology,OntologyTerm-method}
\alias{showPathsToTermById,Ontology,character-method}
\alias{getOntologyRelationNames,Ontology-method}
\alias{getTermRelationNames,Ontology,OntologyTerm-method}
\alias{getTermRelationNamesById,Ontology,character-method}
\alias{getTermRelations,Ontology,OntologyTerm,character-method}
\alias{getTermRelationsById,Ontology,character,character-method}

\title{Class "Ontology" }
\description{Supports basic operations with ontologies: traversal and search}
\section{Accessing the ontologies}{
    The appropriate way to access ontology is via the helper \code{\link{getOntology}} function.
}
\section{Accessing the EFO}{
    The appropriate way to access EFO is via the helper \code{\link{getEFO}} function.
}
\section{Slots}{
    \describe{
        \item{\code{ontology}:}{Object of class \code{"jobjRef"} No user-serviceable parts inside. Maps to an internal Java Ontology object. }
    }
}
\section{Methods}{
    \describe{
        \item{getAllTermChildren}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns list of term's all children }
        \item{getAllTermChildrenById}{\code{signature(object = "Ontology", id = "character")}: Returns list of term's all children }
        \item{getAllTermIds}{\code{signature(object = "Ontology")}: Returns list of all term accessions }
        \item{getAllTermParents}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns list of term's all parents }
        \item{getAllTermParentsById}{\code{signature(object = "Ontology", id = "character")}: Returns list of term's all parents }
        \item{getAllTerms}{\code{signature(object = "Ontology")}: Returns list of all terms }
        \item{getEFOBranchRootIds}{\code{signature(object = "Ontology")}: Returns set of branch root accessions. Method specific for EFO ontology }
        \item{getOntologyAccession}{\code{signature(object = "Ontology")}: Returns parsed ontology accession }
        \item{getOntologyDescription}{\code{signature(object = "Ontology")}: Returns parsed ontology description }
        \item{getRootIds}{\code{signature(object = "Ontology")}: Returns list of root terms accessions, if there are any }
        \item{getRoots}{\code{signature(object = "Ontology")}: Returns list of root terms, if there are any }
        \item{getTermAndAllChildren}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns list of accessions of term itself and all its children recursively }
	\item{getTermAndAllChildrenById}{\code{signature(object = "Ontology", id = "character")}: Returns list of accessions of term itself and all its children recursively }
        \item{getTermById}{\code{signature(object = "Ontology", id = "character")}:  Fetch term by accession. Returns external term representation if found in ontology, null otherwise }
        \item{getTermChildren}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns list of term's direct children }
        \item{getTermChildrenById}{\code{signature(object = "Ontology", id = "character")}: Returns list of term's direct children }
        \item{getTermDefinitions}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns set of term's definitions if there are some }
        \item{getTermNameById}{\code{signature(object = "Ontology", id = "character")}: Returns term's label by accession }
        \item{getTermParents}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}:  Returns list of term's direct parents }
        \item{getTermParentsById}{\code{signature(object = "Ontology", id = "character")}:  Returns list of term's direct parents }
        \item{getTermSynonyms}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns set of term's synonyms  if there are some }
        \item{hasTerm}{\code{signature(object = "Ontology", id = "character")}: Check if term with specified accession exists in ontology }
        \item{isEFOBranchRoot}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns true if term is branch root of EFO. Method specific for EFO ontology }
        \item{isEFOBranchRootById}{\code{signature(object = "Ontology", id = "character")}: Returns true if term is branch root of EFO. Method specific for EFO ontology }
        \item{isRoot}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns true if term is root of ontology}
        \item{isRootById}{\code{signature(object = "Ontology", id = "character")}: Returns true if term is root of ontology}
        \item{searchTerm}{\code{signature(object = "Ontology", id = "character")}: Searches for term in ontology by name }
        \item{searchTermPrefix}{\code{signature(object = "Ontology", prefix = "character")}: Searches for prefix in ontology }
        \item{showHierarchyDownToTerm}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns set of terms that represent ontology "opened" down to specified term, hence displaying all its parents first and then a tree level, containing specified term }
        \item{showHierarchyDownToTermById}{\code{signature(object = "Ontology", id = "character")}: Returns set of terms that represent ontology "opened" down to specified term, hence displaying all its parents first and then a tree level, containing specified term }
        \item{showPathsToTerm}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns paths to the specified term from ontology's root term}
        \item{showPathsToTermById}{\code{signature(object = "Ontology", id = "character")}: Returns paths to the specified term from ontology's root term}
        \item{getOntologyRelationNames}{\code{signature(object = "Ontology")}: Returns list of relations used in ontology}
        \item{getTermRelationNames}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm")}: Returns list of relations that term has}
        \item{getTermRelationNamesById}{\code{signature(object1 = "Ontology", id = "character")}: Returns list of relations that term under given accession has}
        \item{getTermRelations}{\code{signature(object1 = "Ontology", object2 = "OntologyTerm", relation = "character")}: Returns list of terms that are in defined relation with term of interest}
        \item{getTermRelations}{\code{signature(object = "Ontology", id = "character", relation = "character")}: Returns list of terms that are in defined relation with term of interest}         
    }
}
\references{
    Adamusiak T, Burdett T, van der Velde K J, Abeygunawardena N, Antonakaki D, Parkinson H and Swertz M: OntoCAT -- a simpler way to access ontology resources. \emph{Available from Nature Precedings} \url{http://dx.doi.org/10.1038/npre.2010.4666.1} (2010)  
    
    Malone J, Holloway E, Adamusiak T, Kapushesky M, Zheng J, Kolesnikov N, Zhukova A, Brazma A, Parkinson H: Modeling Sample Variables with an Experimental Factor Ontology. \emph{Bioinformatics} 2010, \bold{26}(8):1112--1118 
    
    Experimental Factor Ontology \url{http://www.ebi.ac.uk/efo}
    
    Ontology Common API Tasks java library \url{http://www.ontocat.org}
    
    Java sources and javadocs: \url{http://sourceforge.net/projects/ontocat/files/}
}
\author{Natalja Kurbatova}
\note{This package ships with the EFO OWL file, version released at the time of the package build. Provided EFO OWL file can be loaded as any other OWL or OBO file by using \code{\link{getOntology}} function. 

Another option is to load the latest EFO version on the fly by using \code{\link{getEFO}} function.}

\seealso{
    \code{\link{getOntology}},\code{\link{getEFO}} and \code{\linkS4class{OntologyTerm}}
}

\examples{
    ontology <- getEFO()
    getEFOBranchRootIds(ontology)
    term <- getTermById(ontology,"EFO_0001221")
    getTermParents(ontology,term)
    searchTermPrefix(ontology,"leuk")
    getTermAndAllChildrenById(ontology,"EFO_0000318")
    searchTerm(ontology,"thymus")
    ontology <- getOntology("http://www.ebi.ac.uk/efo/efo.owl")
    ontology <- getOntology("./ontoCAT/extdata/cell.obo")
    getAllTermIds(ontology)
    
}

\keyword{classes}
