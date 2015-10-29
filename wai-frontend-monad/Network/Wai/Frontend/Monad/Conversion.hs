module Network.Wai.Frontend.Monad.Conversion where

import Network.Wai
import Network.Wai.Frontend.Monad.Internal

toRichRequest :: Request -> RichRequest
toRichRequest req = RichRequest
    { rrWaiRequest = req
    }
