module ServerHandler where

import Servant.Checked.Exceptions
import Polysemy.Error
import Polysemy

eitherAddError :: IsMember e es => Either e a -> Envelope es a
eitherAddError = either toErrEnvelope toSuccEnvelope

runErrorToEnv :: IsMember e es => Sem (Error e ': r) a -> Sem r (Envelope es a) 
runErrorToEnv = fmap eitherAddError . runError