module RBPCP.Internal.ServantErrRes where

import Servant.API.ContentTypes

data WithError res err = res `WithError` err

-- instance (MimeRender ctype res, MimeRender ctype err)
--      => MimeRender (WithError res err)
