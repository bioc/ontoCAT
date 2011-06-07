.onLoad <- function(libname, pkgname) {
    #require(rJava)
    .jinit()
    .jaddClassPath(dir(system.file("jars", package=pkgname),full.names=TRUE))
}

getOntology <- function(pathToURI) {
    ontologyFromURI<-.jnew("uk.ac.ebi.ontocat.utils.OntologyParser",pathToURI)
    new("Ontology", ontology=ontologyFromURI)
}

getOntologyNoReasoning <- function(pathToURI) {
    ontologyFromURI<-.jnew("uk.ac.ebi.ontocat.utils.OntologyParser",pathToURI,"false")
    new("Ontology", ontology=ontologyFromURI)
}

getEFO <- function() {
    ontologyEFO<-.jnew("uk.ac.ebi.ontocat.utils.OntologyParser")
    new("Ontology", ontology=ontologyEFO)
}
