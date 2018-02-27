import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Configuration (defaultTestConf, withDefConfigurations)

main :: IO ()
main = do
    putText $ "default configuration: " <> show defaultTestConf
    withDefConfigurations (hspec spec)
