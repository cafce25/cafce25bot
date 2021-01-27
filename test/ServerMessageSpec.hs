module ServerMessageSpec (spec) where
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)

import ServerMessage (ServerMessage(..), Prefix(..), serverMessageP)

spec :: Spec
spec = do
    describe "ServerMessage.serverMessageP" $ do
        it "sholud parse a PING message" $ do
            let msg = "PING :tmi.twitch.tv" 
            parse serverMessageP "" msg `shouldParse` Msg Nothing "PING" ["tmi.twitch.tv"]

        it "should parse a message with 3 digit command and prefix" $ do
            let msg = ":tmi.twitch.tv 001 cafce25bot :Welcome, GLHF!"
            parse serverMessageP "" msg `shouldParse` Msg (Just $ Prefix Nothing "tmi.twitch.tv") "001" ["cafce25bot", "Welcome, GLHF!"]

        it "should parse a JOIN message" $ do
            let user = "cafce25bot"
                channel = "cafce25"
                msg = ":" ++ user ++ "!" ++ user ++ "@" ++ user ++ ".tmi.twitch.tv JOIN #" ++ channel
            parse serverMessageP "" msg `shouldParse` Msg (Just $ Prefix (Just user) "tmi.twitch.tv") "JOIN" ['#':channel]
