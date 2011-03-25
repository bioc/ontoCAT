#Licensed to the Apache Software Foundation (ASF) under one
#or more contributor license agreements.  See the NOTICE file
#distributed with this work for additional information
#regarding copyright ownership.  The ASF licenses this file
#to you under the Apache License, Version 2.0 (the
#"License"); you may not use this file except in compliance
#with the License.  You may obtain a copy of the License at

#http://www.apache.org/licenses/LICENSE-2.0

#Unless required by applicable law or agreed to in writing,
#software distributed under the License is distributed on an
#"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#KIND, either express or implied.  See the License for the
#specific language governing permissions and limitations
#under the License.

setClass(Class="OntologyTerm", representation=representation(term="jobjRef"))

setGeneric(name="getAccession",                    def=function(object){standardGeneric("getAccession")})
setGeneric(name="getLabel",                  def=function(object){standardGeneric("getLabel")})

setMethod("show",          "OntologyTerm", function(object) cat(object@term$getAccession(), ": ", object@term$getLabel(), "\n", sep=""))
setMethod("getAccession",         "OntologyTerm", function(object) object@term$getAccession())
setMethod("getLabel",       "OntologyTerm", function(object) object@term$getLabel())

setClass(Class="Ontology",  representation=representation(ontology="jobjRef"))

setGeneric(name="getAllTermChildrenById",              def=function(object, id)       {standardGeneric("getAllTermChildrenById")})
setGeneric(name="getAllTermChildren",              def=function(object1, object2)       {standardGeneric("getAllTermChildren")})
setGeneric(name="getAllTermIds",            def=function(object)       {standardGeneric("getAllTermIds")})
setGeneric(name="getAllTermParentsById",              def=function(object, id)       {standardGeneric("getAllTermParentsById")})
setGeneric(name="getAllTermParents",              def=function(object1, object2)       {standardGeneric("getAllTermParents")})
setGeneric(name="getAllTerms",              def=function(object)       {standardGeneric("getAllTerms")})
setGeneric(name="getTermDefinitionsById",              def=function(object, id)       {standardGeneric("getTermDefinitionsById")})
setGeneric(name="getTermDefinitions",              def=function(object1, object2)       {standardGeneric("getTermDefinitions")})
setGeneric(name="getEFOBranchRootIds",         def=function(object)       {standardGeneric("getEFOBranchRootIds")})
setGeneric(name="getOntologyAccession",              def=function(object)       {standardGeneric("getOntologyAccession")})
setGeneric(name="getOntologyDescription",              def=function(object)       {standardGeneric("getOntologyDescription")})
setGeneric(name="getRootIds",               def=function(object)       {standardGeneric("getRootIds")})
setGeneric(name="getRoots",                 def=function(object)       {standardGeneric("getRoots")})
setGeneric(name="getTermSynonymsById",               def=function(object, id)       {standardGeneric("getTermSynonymsById")})
setGeneric(name="getTermSynonyms",               def=function(object1, object2)       {standardGeneric("getTermSynonyms")})
setGeneric(name="getTermAndAllChildrenById", def=function(object, id)   {standardGeneric("getTermAndAllChildrenById")})
setGeneric(name="getTermAndAllChildren", def=function(object1, object2)   {standardGeneric("getTermAndAllChildren")})
setGeneric(name="getTermById",              def=function(object, id)   {standardGeneric("getTermById")})
setGeneric(name="getTermChildrenById",          def=function(object, id)   {standardGeneric("getTermChildrenById")})
setGeneric(name="getTermChildren",          def=function(object1, object2)   {standardGeneric("getTermChildren")})
setGeneric(name="getTermNameById",          def=function(object, id)   {standardGeneric("getTermNameById")})
setGeneric(name="getTermParentsById",           def=function(object, id)   {standardGeneric("getTermParentsById")})
setGeneric(name="getTermParents",           def=function(object1, object2)   {standardGeneric("getTermParents")})
setGeneric(name="hasTerm",                  def=function(object, id)   {standardGeneric("hasTerm")})
setGeneric(name="isEFOBranchRootById",             def=function(object, id)       {standardGeneric("isEFOBranchRootById")})
setGeneric(name="isEFOBranchRoot",             def=function(object1, object2)       {standardGeneric("isEFOBranchRoot")})
setGeneric(name="isRootById",                   def=function(object, id)       {standardGeneric("isRootById")})
setGeneric(name="isRoot",                   def=function(object1, object2)       {standardGeneric("isRoot")})
setGeneric(name="searchTerm",         def=function(object, id) {standardGeneric("searchTerm")})
setGeneric(name="searchTermPrefix",         def=function(object, prefix) {standardGeneric("searchTermPrefix")})
setGeneric(name="showHierarchyDownToTermById",         def=function(object, id) {standardGeneric("showHierarchyDownToTermById")})
setGeneric(name="showHierarchyDownToTerm",         def=function(object1, object2) {standardGeneric("showHierarchyDownToTerm")})
setGeneric(name="showPathsToTermById",         def=function(object, id) {standardGeneric("showPathsToTermById")})
setGeneric(name="showPathsToTerm",         def=function(object1, object2) {standardGeneric("showPathsToTerm")})
#Relations
setGeneric(name="getOntologyRelationNames",               def=function(object)       {standardGeneric("getOntologyRelationNames")})
setGeneric(name="getTermRelationNames",           def=function(object1, object2)   {standardGeneric("getTermRelationNames")})
setGeneric(name="getTermRelationNamesById",           def=function(object, id)   {standardGeneric("getTermRelationNamesById")})
setGeneric(name="getTermRelations",           def=function(object1, object2, relation)   {standardGeneric("getTermRelations")})
setGeneric(name="getTermRelationsById",           def=function(object, id, relation)   {standardGeneric("getTermRelationsById")})


setMethod(f="getAllTermChildrenById", signature=c(object="Ontology", id="character"), definition=function(object, id) {
    x <- ((object@ontology)$getAllTermChildren(id))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getAllTermChildren", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) {
    x <- ((object1@ontology)$getAllTermChildren(object2@term))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getAllTermIds", "Ontology", function(object) {
    x <- (object@ontology)$getAllTermIds()
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getAllTermParentsById", signature=c(object="Ontology", id="character"), definition=function(object, id) {
    x <- ((object@ontology)$getAllTermParents(id))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getAllTermParents", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) {
    x <- ((object1@ontology)$getAllTermParents(object2@term))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getAllTerms", "Ontology", function(object) {
    x <- (object@ontology)$getAllTerms()
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTermDefinitionsById", signature=c(object="Ontology", id="character"), definition=function(object, id) {
    x <- ((object@ontology)$getTermDefinitions(id))
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getTermDefinitions", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) {
    x <- ((object1@ontology)$getTermDefinitions(object2@term))
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getEFOBranchRootIds", "Ontology", definition=function(object) {
    x <- ((object@ontology)$getEFOBranchRootIds())
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getOntologyAccession", "Ontology",   definition=function(object) (object@ontology)$getOntologyAccession())
setMethod(f="getOntologyDescription", "Ontology",   definition=function(object) (object@ontology)$getOntologyDescription())
setMethod(f="getRootIds", "Ontology", definition=function(object) {
    x <- ((object@ontology)$getRootIds())
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getRoots", "Ontology", definition=function(object) {
    x <- ((object@ontology)$getRoots())
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTermSynonymsById", signature=c(object="Ontology", id="character"), definition=function(object, id) {
    x <- ((object@ontology)$getTermSynonyms(id))
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getTermSynonyms", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) {
    x <- ((object1@ontology)$getTermSynonyms(object2@term))
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getTermAndAllChildrenById", signature=c(object="Ontology", id="character"), definition=function(object, id) {
    x <- ((object@ontology)$getTermAndAllChildren(id))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTermAndAllChildren", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) {
    x <- ((object1@ontology)$getTermAndAllChildren(object2@term))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTermById", signature=c(object="Ontology", id="character"),   definition=function(object, id) {
    term <- (object@ontology)$getTermById(id)
    new("OntologyTerm", term=term)
})
setMethod(f="getTermChildrenById", signature=c(object="Ontology", id="character"), definition=function(object, id) {
    x <- ((object@ontology)$getTermChildren(id))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTermChildren", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) {
    x <- ((object1@ontology)$getTermChildren(object2@term))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTermNameById", signature=c(object="Ontology", id="character"),   definition=function(object, id) (object@ontology)$getTermNameById(id))
setMethod(f="getTermParentsById", signature=c(object="Ontology", id="character"), definition=function(object, id) {
    x <- ((object@ontology)$getTermParents(id))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTermParents", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) {
    x <- ((object1@ontology)$getTermParents(object2@term))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="hasTerm",         signature=c(object="Ontology", id="character"),   definition=function(object, id) (object@ontology)$hasTerm(id))
setMethod(f="isEFOBranchRootById",         signature=c(object="Ontology", id="character"),   definition=function(object, id) (object@ontology)$isEFOBranchRoot(id))
setMethod(f="isEFOBranchRoot",         signature=c(object1="Ontology", object2="OntologyTerm"),  definition=function(object1, object2) (object1@ontology)$isEFOBranchRoot(object2@term))
setMethod(f="isRootById",         signature=c(object="Ontology", id="character"),   definition=function(object, id) (object@ontology)$isRoot(id))
setMethod(f="isRoot",         signature=c(object1="Ontology", object2="OntologyTerm"),   definition=function(object1, object2) (object1@ontology)$isRoot(object2@term))
setMethod(f="searchTerm", signature=c(object="Ontology", id="character"), definition=function(object, id) {
    x <- (object@ontology)$searchTerm(id)
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="searchTermPrefix", signature=c(object="Ontology", prefix="character"), definition=function(object, prefix) {
    x <- (object@ontology)$searchTermPrefix(prefix)
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="showHierarchyDownToTermById", signature=c(object="Ontology", id="character"), definition=function(object, id) (object@ontology)$showHierarchyDownToTerm(id))
setMethod(f="showHierarchyDownToTerm", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) (object1@ontology)$showHierarchyDownToTerm(object2@term))
setMethod(f="showPathsToTermById", signature=c(object="Ontology", id="character"), definition=function(object, id) (object@ontology)$showPathsToTerm(id))
setMethod(f="showPathsToTerm", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) (object1@ontology)$showPathsToTerm(object2@term))

#Relations
setMethod(f="getOntologyRelationNames", "Ontology", definition=function(object) {
    x <- ((object@ontology)$getOntologyRelationNames())
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getTermRelationNamesById", signature=c(object="Ontology", id="character"), definition=function(object, id) {
    x <- ((object@ontology)$getTermRelationNames(id))
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getTermRelationNames", signature=c(object1="Ontology", object2="OntologyTerm"), definition=function(object1, object2) {
    x <- ((object1@ontology)$getTermRelationNames(object2@term))
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getTermRelationsById", signature=c(object="Ontology", id="character", relation="character"), definition=function(object, id, relation) {
    x <- ((object@ontology)$getTermRelations(id,relation))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTermRelations", signature=c(object1="Ontology", object2="OntologyTerm", relation="character"), definition=function(object1, object2, relation) {
    x <- ((object1@ontology)$getTermRelations(object2@term,relation))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})