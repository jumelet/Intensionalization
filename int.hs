{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, DeriveDataTypeable #-}

{-
Based on:

@Article{deGroote2013,
  author="de Groote, Philippe
  and Kanazawa, Makoto",
  title="A Note on Intensionalization",
  journal="Journal of Logic, Language and Information",
  year="2013",
  volume="22",
  number="2",
  pages="173--194",
  abstract="Building on Ben-Avi and Winter's (2007) work, this paper provides a general ``intensionalization'' procedure that turns an extensional semantics for a language into an intensionalized one that is capable of accommodating ``truly intensional'' lexical items without changing the compositional semantic rules. We prove some formal properties of this procedure and clarify its relation to the procedure implicit in Montague's (1973) PTQ.",
  issn="1572-9583",
  doi="10.1007/s10849-013-9173-9",
  url="http://dx.doi.org/10.1007/s10849-013-9173-9"
}

-}

import Data.Typeable

type T = Bool

newtype S = World Int
  deriving (Eq,Ord,Enum,Typeable)

data E = John
  deriving (Eq,Show,Enum,Bounded,Typeable)


int_a :: Type a => (S -> a) -> (S -> a) 
int_a = id 

int_ab :: (Type a, Type b) => (S -> a -> b) -> (Intensionalized a -> Intensionalized b)
int_ab x y = int (\i -> x i (ext y i))
 
 
ext_a :: Type a => (S -> a) -> (S -> a)
ext_a = id 
 
ext_ab :: (Type a, Type b) => (Intensionalized a -> Intensionalized b) -> S -> a -> b
ext_ab y j x = ext (y (int (\k -> x))) j             

test :: S -> (E -> T)
test = undefined


class Typeable a => Type a  where
  type Intensionalized a
  int :: (S -> a) -> Intensionalized a
  ext :: Intensionalized a -> S -> a

instance Type E where
  type Intensionalized E = S -> E
  int = int_a
  ext = ext_a
  
instance Type T where
  type Intensionalized T = S -> T
  int = int_a
  ext = ext_a
  
instance Type S where
  type Intensionalized S = S -> S
  int = int_a
  ext = ext_a
  
instance (Type a ,Type b ) => Type (a->b) where
  type Intensionalized (a->b) = (Intensionalized a -> Intensionalized b)
  int = int_ab
  ext = ext_ab  
  
  
  
  