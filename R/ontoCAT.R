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

setClass(Class="OntologyParser",  representation=representation(op="jobjRef"))

setGeneric(name="getAllTermChildren",              def=function(object, id)       {standardGeneric("getAllTermChildren")})
setGeneric(name="getAllTermIds",            def=function(object)       {standardGeneric("getAllTermIds")})
setGeneric(name="getAllTermParents",              def=function(object, id)       {standardGeneric("getAllTermParents")})
setGeneric(name="getAllTerms",              def=function(object)       {standardGeneric("getAllTerms")})
setGeneric(name="getDefinitions",              def=function(object, id)       {standardGeneric("getDefinitions")})
setGeneric(name="getEFOBranchRootIds",         def=function(object)       {standardGeneric("getEFOBranchRootIds")})
setGeneric(name="getOntologyAccession",              def=function(object)       {standardGeneric("getOntologyAccession")})
setGeneric(name="getOntologyDescription",              def=function(object)       {standardGeneric("getOntologyDescription")})
setGeneric(name="getRootIds",               def=function(object)       {standardGeneric("getRootIds")})
setGeneric(name="getRoots",                 def=function(object)       {standardGeneric("getRoots")})
setGeneric(name="getSynonyms",               def=function(object, id)       {standardGeneric("getSynonyms")})
setGeneric(name="getTermAndAllChildrenIds", def=function(object, id)   {standardGeneric("getTermAndAllChildrenIds")})
setGeneric(name="getTermById",              def=function(object, id)   {standardGeneric("getTermById")})
setGeneric(name="getTermChildren",          def=function(object, id)   {standardGeneric("getTermChildren")})
setGeneric(name="getTermNameById",          def=function(object, id)   {standardGeneric("getTermNameById")})
setGeneric(name="getTermParents",           def=function(object, id)   {standardGeneric("getTermParents")})
setGeneric(name="getTreeDownTo",            def=function(object, id)    {standardGeneric("getTreeDownTo")})
setGeneric(name="hasTerm",                  def=function(object, id)   {standardGeneric("hasTerm")})
setGeneric(name="isEFOBranchRoot",             def=function(object, id)       {standardGeneric("isEFOBranchRoot")})
setGeneric(name="isRoot",                   def=function(object, id)       {standardGeneric("isRoot")})
setGeneric(name="searchTerm",         def=function(object, id) {standardGeneric("searchTerm")})
#setGeneric(name="searchTermInBioportal",         def=function(object, id) {standardGeneric("searchTermInBioportal")})
#setGeneric(name="searchTermInOLS",         def=function(object, id) {standardGeneric("searchTermInOLS")})
setGeneric(name="searchTermPrefix",         def=function(object, prefix) {standardGeneric("searchTermPrefix")})


setMethod(f="getAllTermChildren", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
    x <- ((object@op)$getAllTermChildren(id))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getAllTermIds", "OntologyParser", function(object) {
    x <- (object@op)$getAllTermIds()
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getAllTermParents", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
    x <- ((object@op)$getAllTermParents(id))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getAllTerms", "OntologyParser", function(object) {
    x <- (object@op)$getAllTerms()
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getDefinitions", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
    x <- ((object@op)$getDefinitions(id))
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getEFOBranchRootIds", "OntologyParser", definition=function(object) {
    x <- ((object@op)$getEFOBranchRootIds())
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getOntologyAccession", "OntologyParser",   definition=function(object) (object@op)$getOntologyAccession())
setMethod(f="getOntologyDescription", "OntologyParser",   definition=function(object) (object@op)$getOntologyDescription())
setMethod(f="getRootIds", "OntologyParser", definition=function(object) {
    x <- ((object@op)$getRootIds())
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getRoots", "OntologyParser", definition=function(object) {
    x <- ((object@op)$getRoots())
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getSynonyms", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
    x <- ((object@op)$getSynonyms(id))
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getTermAndAllChildrenIds", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
    x <- ((object@op)$getTermAndAllChildrenIds(id))
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
setMethod(f="getTermById", signature=c(object="OntologyParser", id="character"),   definition=function(object, id) {
    term <- (object@op)$getTermById(id)
    new("OntologyTerm", term=term)
})
setMethod(f="getTermChildren", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
    x <- ((object@op)$getTermChildren(id))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTermNameById", signature=c(object="OntologyParser", id="character"),   definition=function(object, id) (object@op)$getTermNameById(id))
setMethod(f="getTermParents", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
    x <- ((object@op)$getTermParents(id))
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="getTreeDownTo", signature=c(object="OntologyParser", id="character"), definition=function(object,id) {
    x <- (object@op)$getTreeDownTo(id)
    sapply(.jevalArray(x$toArray()),function(term) new("OntologyTerm", term=term))
})
setMethod(f="hasTerm",         signature=c(object="OntologyParser", id="character"),   definition=function(object, id) (object@op)$hasTerm(id))
setMethod(f="isEFOBranchRoot",         signature=c(object="OntologyParser", id="character"),   definition=function(object, id) (object@op)$isEFOBranchRoot(id))
setMethod(f="isRoot",         signature=c(object="OntologyParser", id="character"),   definition=function(object, id) (object@op)$isRoot(id))
setMethod(f="searchTerm", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
    x <- (object@op)$searchTerm(id)
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})
#setMethod(f="searchTermInBioportal", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
#    x <- (object@op)$searchTermInBioportal(id)
#    sapply(.jevalArray(x$toArray()),function(id) id$toString())
#})
#setMethod(f="searchTermInOLS", signature=c(object="OntologyParser", id="character"), definition=function(object, id) {
#    x <- (object@op)$searchTermInOLS(id)
#    sapply(.jevalArray(x$toArray()),function(id) id$toString())
#})
setMethod(f="searchTermPrefix", signature=c(object="OntologyParser", prefix="character"), definition=function(object, prefix) {
    x <- (object@op)$searchTermPrefix(prefix)
    sapply(.jevalArray(x$toArray()),function(id) id$toString())
})