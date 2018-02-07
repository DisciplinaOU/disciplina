
module Disciplina.Core.IndexedWithProof where

import Universum

{-|
    This is an interface for tree-that-provide-proofs, to abstract from
    its implementation AND change it if we desire so w/o changing business logic,
    provided the proof structure is the same.

    The implementations are planned for:
    - Data.Tree.AVL from AVL;
    - Future tree-like storage made on some database.

 -}
class MonadWriter (Proofs t) m => IndexedWithProof t m | t -> m where
    type Key    t :: *
    type Value  t :: *
    type Proof  t :: *
    type Proofs t :: *
    type Proofs t = [Proof t]

    initialize ::                          m  t
    retrieve   :: t -> Key t ->            m (Maybe (Value t))
    upsert     :: t -> Key t -> Value t -> m ()
    remove     :: t -> Key t ->            m (Bool)
