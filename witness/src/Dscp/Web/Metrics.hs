module Dscp.Web.Metrics
    ( MetricsEndpoint (..)
    , responseTimeMetric
    , reportTime
    ) where

import Mon (recordTimer)
import Mon.Network (Endpoint)
import Mon.Types (Name)
import Network.Wai (Middleware)

import Dscp.Util (countingTime)

newtype MetricsEndpoint = MetricsEndpoint { unMetricsEndpoint :: Maybe Endpoint }
    deriving (Show)

reportTime :: MonadIO m => Name -> MetricsEndpoint -> m a -> m a
reportTime name (MetricsEndpoint mEndpoint) m = case mEndpoint of
    Nothing -> m
    Just endpoint -> do
        (time, a) <- countingTime m
        -- mon accepts only Int as metric value and expects amount of milliseconds in recordTimer
        liftIO $ recordTimer endpoint name 1 [] (round $ time * 1000)
        return a

responseTimeMetric :: MetricsEndpoint -> Middleware
responseTimeMetric endpoint app = \request f ->
    reportTime "disciplina.timer.http_request" endpoint (app request f)
