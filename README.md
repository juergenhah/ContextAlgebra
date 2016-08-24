# ContextAlgebra
Implementation of the ContextAlgebra published in dissertation: *Context Algebra applied to Spatial Concepts*

This library is available on hackage [ContextAlgebra](https://hackage.haskell.org/package/ContextAlgebra)

# Abstract
Words are used to refer to objects in reality. One word can imply many references to different objects that are categorized as similar. For example, the word “city” can refer to Vienna, Alexandria, or Las Vegas; the word “near” can refer to a range of distances, e. g. “moon is near the earth” or “near St. Stephens cathedral”. If a Geographic Information System (GIS) is queried with a sentence including “city” or “near”, the challenge for an algorithm executed by the GIS is to decide which exemplar of the word “city” or which distance “near” refers to.

To overcome this challenge, the hypothesis is that context selects references to objects in reality. A context algebra is presented, implemented, and used to represent the word “near” (in the thesis), in order to evaluate the hypothesis. Context algebra makes use of the theory established by a context-enriched semiotic triangle. The semiotic triangle connects objects in reality to words via concepts in an agent. With context enrichment, the concept is separated into contextualized concepts that include objects in reality valid for a specific context. If words are used in this specific context, then they correspond to a specific contextualized concept, which then selects specific references to objects from reality.

Context algebra proposes a formalization for context. In this algebra, contexts are ordered with a partial order relation and can be combined with a disjunction or conjunction function to create other contexts. This relation and these functions satisfy algebraic properties that result in a lattice structure for context. Each context included in the lattice is mapped to a contextualized concept. A contextualized concept is modeled with sets of objects observed from reality, where a typical object is determined. This typical object (prototype) is assumed to be the translation from a word to an object in reality. For example, the influencing context “capital of Austria” for the word “city” selects the prototypical instance Vienna. 

Context algebra is implemented using Haskell, it is then proven, and the complexity class is determined. 
 - The implementation shows that the context algebra is realizable with reasonable performance. 
 - To prove that the implementation satisfies the proposed algebraic properties, tests are successfully executed. 
 - The benchmarks determine the complexity class exponential for the implementation.


# Implementation
The implementation is structured into several modules according to the mathematical structure of the theory.

 - **ContextAlgebra** is built upon several abstract classes, each representing a mathematical structure.
 - **Concept** invents two containers, one to connect a concept to context (*Observation* data type) and one to build the concept (*MultiSet* of *Observations*).
 - **Mapping** establishes a *ContextualizedConcept* via the mapping function *m*
 - **ContextualizedConcept** calculates prototypes for a *ContextualizedConcept*.

## SetImplementation
 - **ContextAlgebraSetInstance** instances all abstract classes given in module **ContextAlgebra** using sets.

## ExemplarScales
 - **NominalExemplars** calculates a prototype from exemplars on a nominal exemplar scales.
 - **RatioExamplars** calculates a prototype from exemplars on a rational measurement scale.

# Evaluation 
 - **ContextAlgebraEvaluation** execution of all property tests for the **ContextAlgebra**.
 - **QuickCheckHelper** wrapper functions for [QuickCheck](https://hackage.haskell.org/package/QuickCheck) execution.

# Benchmarks - Complexity
 - **ConceptGenerator** establishes concepts, with a freely selected number of exemplars and contexts.
 - **ProtoCalculator** calculates prototypes for each level of a *ContextLattice*.

# Example for a General Context Operation, full explanation on page 58 in dissertation

Module **GeneralContextOperation** shows an example of the context algebra refining concepts and calculating prototypes for contextualized concepts.


# Further Reading
If you are interested, there are two publications online to read and do not hesitate to contact me.
 - Context Algebra applied to Spatial Concepts (soon available)
 - [A Computational Model for Context and Spatial Concepts](https://link.springer.com/chapter/10.1007/978-3-319-33783-8_1)

