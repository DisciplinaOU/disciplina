{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

-- | Some utilites for more flexible servant usage.
-- Copy-pasted with minor changes from my work from /you know there/ (@martoon).

module Dscp.Util.Servant
    ( CanHoistClient (..)
    ) where

import Prelude hiding (log)

import GHC.TypeLits (Symbol)
import Network.HTTP.Types.Method (StdMethod)
import Servant.API ((:<|>) (..), (:>), Capture, Description, QueryFlag, QueryParam, ReqBody,
                    Summary, Verb)
import Servant.Client.Core (Client)
import Servant.Util (PaginationParams, SortingParams)

-- Please anybody switch us to lts-12 already so that there is no need in this:
class CanHoistClient m api where
    hoistClientMonad :: Proxy m -> Proxy api -> (forall a. m a -> n a) -> Client m api -> Client n api

instance CanHoistClient m api => CanHoistClient m ((path :: Symbol) :> api) where
    hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)
instance CanHoistClient m api =>
         CanHoistClient m (Capture name a :> api) where
    hoistClientMonad pm _ hst cli arg = hoistClientMonad pm (Proxy @api) hst (cli arg)
instance CanHoistClient m api =>
         CanHoistClient m (QueryParam name a :> api) where
    hoistClientMonad pm _ hst cli arg = hoistClientMonad pm (Proxy @api) hst (cli arg)
instance CanHoistClient m api =>
         CanHoistClient m (QueryFlag name :> api) where
    hoistClientMonad pm _ hst cli arg = hoistClientMonad pm (Proxy @api) hst (cli arg)
instance CanHoistClient m api =>
         CanHoistClient m (ReqBody (ct ': cts) a :> api) where
    hoistClientMonad pm _ hst cli arg = hoistClientMonad pm (Proxy @api) hst (cli arg)
instance CanHoistClient m api =>
         CanHoistClient m (SortingParams params :> api) where
    hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)
instance CanHoistClient m api =>
         CanHoistClient m (PaginationParams :> api) where
    hoistClientMonad pm _ hst cli arg = hoistClientMonad pm (Proxy @api) hst (cli arg)
instance CanHoistClient m api =>
         CanHoistClient m (Summary msg :> api) where
    hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)
instance CanHoistClient m api =>
         CanHoistClient m (Description msg :> api) where
    hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)
instance (CanHoistClient m api1, CanHoistClient m api2) =>
         CanHoistClient m (api1 :<|> api2) where
    hoistClientMonad pm _ hst (cli1 :<|> cli2) =
        hoistClientMonad pm (Proxy @api1) hst cli1
        :<|>
        hoistClientMonad pm (Proxy @api2) hst cli2
instance CanHoistClient m (Verb (method :: StdMethod) statusCode contentTypes a) where
    hoistClientMonad _ _ hst cli = hst cli
