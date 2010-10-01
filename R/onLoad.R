.onLoad <- function(libname, pkgname) {
    #require(rJava)
    .jinit()
    .jaddClassPath(dir(system.file("jars", package="ontoCAT"),full.names=TRUE))
}

getOntologyParser <- function(pathToURI) {
    ontologyParser<-.jnew("uk.ac.ebi.ontocat.OntologyParser",pathToURI)
    new("OntologyParser", op=ontologyParser)
}

getEFOParser <- function() {
    ontologyParser<-.jnew("uk.ac.ebi.ontocat.OntologyParser")
    new("OntologyParser", op=ontologyParser)
}