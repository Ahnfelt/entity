
Entity - A Haskell game state library
=====================================

While it may seem intuitive to find a reasonable way to model game state in many imperative languages, we have found this somewhat harder to do in Haskell. This library provides a way to combine stateful *features* to form entities. This allows one to write generic functions able to operate on all entities haven a certain subset of features. 

The library is not pure in the sense that *STM* is used to handle state. The type of an entity is the set of all its features types. This is achieved using *HList*. *fclabels* is used as state containers to gain first class setter functions. These records must derive *Typeable* as we use dynamics internally to decompose entities. 

Currently the library only consists of a single file which are packaged together with several showcase examples.  
