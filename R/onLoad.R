.onLoad <- function(libname, pkgname) {
    #require(rJava)
    .jinit()
    .jaddClassPath(dir(system.file("jars", package="ontoCAT"),full.names=TRUE))
}

getOntology <- function(pathToURI) {
    ontology<-.jnew("uk.ac.ebi.ontocat.utils.OntologyParser",pathToURI)
    new("Ontology", op=ontology)
}

getEFO <- function() {
    ontologyParser<-.jnew("uk.ac.ebi.ontocat.utils.OntologyParser")
    new("Ontology", op=ontologyParser)
}