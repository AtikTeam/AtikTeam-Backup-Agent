module Main (main) where

import Backup.Form as Form
import Backup.Maintenance (expiredSnapshots, needsBackup)
import Backup.Messages as Msg
import Backup.Time (parseSnapshotName, snapshotName)
import Backup.Types as Types
import Data.Aeson (decode, eitherDecode, encode)


import Data.List (singleton)
import Data.Time
    ( DayOfWeek(Monday, Tuesday, Sunday),
      LocalTime(LocalTime),
      fromGregorian,
      TimeOfDay(TimeOfDay) )
import Test.Hspec ( hspec, describe, it, shouldBe, shouldNotBe )

-- | Local time, to the second.
localAt :: Integer -> Int -> Int -> Int -> Int -> Int -> LocalTime
localAt year month day hour minute second =
  LocalTime (fromGregorian year month day) (TimeOfDay hour minute (fromIntegral second))

sampleConf :: Types.InstanceConf
sampleConf =
  Types.InstanceConf
    { confHost = Types.BackupHost "backup.example.com" 443 ""
    , confProxy = Nothing
    , confAuthToken = "tok"
    , confBackupDays = [Tuesday]
    , confHistorySize = 5
    }

main :: IO ()
main = hspec $ do
  describe "Backup.Time" $
    it "round-trips a snapshot name, in the format used since the first version" $ do
      let t = localAt 2026 10 6 13 59 3
      snapshotName t `shouldBe` "2026-10-06---13-59-03"
      parseSnapshotName (snapshotName t) `shouldBe` Just t

  describe "Backup.Types" $ do
    it "reads a configuration written by a previous version (with the old secure field)" $
      eitherDecode
        "{\"backup-host\":{\"domain\":\"backup.example.com\",\"port\":443,\"prefix\":\"\",\"secure\":true},\
        \\"proxy\":null,\"auth-token\":\"tok\",\"backup-days\":[\"Tuesday\"],\"history-size\":5}"
        `shouldBe` Right sampleConf
    it "round-trips a configuration with a proxy" $ do
      let conf = sampleConf {confProxy = Just (Types.ProxyConf "proxy.example.com" 3128)}
      decode (encode conf) `shouldBe` Just conf
    it "reads a sitemap" $ do
      let expected = singleton $
            Types.Resource { resEtag = "abc"
                           , resSource = Types.Internal
                           , resUrl = "/index.html"
                           , resPath = "/index.html"
                           , resHeaders = [("Content-Type", "text/html")]
                           } 

      eitherDecode
        "[[\"abc\",\"internal\",\"/index.html\",\"/index.html\",[[\"Content-Type\",\"text/html\"]]]]"
        `shouldBe` Right expected
      decode (encode expected) `shouldBe` Just expected
    it "validates application names" $ do
      Types.isValidInstanceName "demo.example-1_x" `shouldBe` True
      Types.isValidInstanceName "_" `shouldBe` False
      Types.isValidInstanceName ".hidden" `shouldBe` False
      Types.isValidInstanceName "../etc" `shouldBe` False
      Types.isValidInstanceName "a/b" `shouldBe` False
      Types.isValidInstanceName "" `shouldBe` False

  describe "Backup.Form" $ do
    it "creates an application from the backup identifier" $
      fmap
        (\(name, conf) -> (name, confAuthToken conf, confBackupDays conf))
        (Form.newInstanceFromForm Tuesday [("full-token", "demo/secret")])
        `shouldBe` Right ("demo", "secret", [Tuesday])
    it "rejects an incorrect identifier" $ do
      fmap fst (Form.newInstanceFromForm Tuesday [("full-token", "../x/y")]) `shouldBe` Left Form.InvalidName
      fmap fst (Form.newInstanceFromForm Tuesday [("full-token", "demo")]) `shouldBe` Left Form.MalformedToken
      fmap fst (Form.newInstanceFromForm Tuesday [("full-token", " ")]) `shouldBe` Left Form.MissingToken
      fmap fst (Form.newInstanceFromForm Tuesday []) `shouldBe` Left Form.MissingToken
    it "updates the settings" $ do
      let form =
            [ ("backup-day", "Monday")
            , ("backup-day", "Monday")
            , ("backup-day", "Sunday")
            , ("history-size", "3")
            , ("proxy-host", "p.example.com")
            , ("proxy-port", "3128")
            ]
          conf = Form.updateConfFromForm form sampleConf
      confBackupDays conf `shouldBe` [Monday, Sunday]
      confHistorySize conf `shouldBe` 3
      confProxy conf `shouldBe` Just (Types.ProxyConf "p.example.com" 3128)
    it "keeps the previous history size when the new one is zero or invalid" $ do
      confHistorySize (Form.updateConfFromForm [("history-size", "0")] sampleConf) `shouldBe` 5
      confHistorySize (Form.updateConfFromForm [("history-size", "x")] sampleConf) `shouldBe` 5
    it "removes the proxy when one of its fields is empty" $
      confProxy (Form.updateConfFromForm [("proxy-host", "p"), ("proxy-port", "")] sampleConf)
        `shouldBe` Nothing

  describe "Backup.Maintenance" $ do
    -- 6 October 2026 is a Tuesday
    let now = localAt 2026 10 6 10 0 0
    it "backs up on a scheduled day when nothing was done today" $ do
      needsBackup now sampleConf Nothing `shouldBe` True
      needsBackup now sampleConf (Just (localAt 2026 10 5 23 59 0)) `shouldBe` True
    it "does not back up twice on the same day" $
      needsBackup now sampleConf (Just (localAt 2026 10 6 1 0 0)) `shouldBe` False
    it "does not back up on an unscheduled day" $
      needsBackup now sampleConf {confBackupDays = [Monday]} Nothing `shouldBe` False
    it "keeps the most recent snapshots" $ do
      let old = localAt 2026 10 1 0 0 0
          mid = localAt 2026 10 2 0 0 0
          recent = localAt 2026 10 3 0 0 0
      expiredSnapshots 2 [mid, recent, old] `shouldBe` [old]
      expiredSnapshots 5 [mid, recent, old] `shouldBe` []
      -- never fewer than one snapshot kept
      expiredSnapshots 0 [mid, recent, old] `shouldBe` [mid, old]

  describe "Backup.Messages" $ do
    it "round-trips language codes" $
      map (Msg.parseLang . Msg.langCode) [minBound .. maxBound] `shouldBe` map Just [Msg.English, Msg.French]
    it "rejects an unknown language code" $
      Msg.parseLang "de" `shouldBe` Nothing
    it "formats the date of a snapshot in each language" $ do
      let t = localAt 2026 10 6 13 59 3
      Msg.render Msg.English (Msg.MsgSnapshotDate t) `shouldBe` "2026-10-06 13:59"
      Msg.render Msg.French (Msg.MsgSnapshotDate t) `shouldBe` "le 06-10-2026 à 13:59"
    it "translates messages with parameters" $ do
      Msg.render Msg.English (Msg.MsgBackupsOf "demo") `shouldBe` "Backups of demo"
      Msg.render Msg.French (Msg.MsgBackupsOf "demo") `shouldBe` "Sauvegardes de demo"
      Msg.render Msg.French (Msg.MsgDay Monday) `shouldBe` "Lundi"
    it "has a different text in each language for every day" $
      mapM_
        (\day -> Msg.render Msg.English (Msg.MsgDay day) `shouldNotBe` Msg.render Msg.French (Msg.MsgDay day))
        Types.allDays
