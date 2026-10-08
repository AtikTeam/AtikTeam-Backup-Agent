module Main (main) where

import Backup.Form
import Backup.Maintenance (expiredSnapshots, needsBackup)
import Backup.Messages
import Backup.Time (parseSnapshotName, snapshotName)
import Backup.Types
import Data.Aeson (decode, eitherDecode, encode)
-- import Data.Either (isLeft)
import Data.Time
import Test.Hspec

-- | Local time, to the second.
localAt :: Integer -> Int -> Int -> Int -> Int -> Int -> LocalTime
localAt year month day hour minute second =
  LocalTime (fromGregorian year month day) (TimeOfDay hour minute (fromIntegral second))

sampleConf :: InstanceConf
sampleConf =
  InstanceConf
    { confHost = BackupHost "backup.example.com" 443 ""
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
      let conf = sampleConf {confProxy = Just (ProxyConf "proxy.example.com" 3128)}
      decode (encode conf) `shouldBe` Just conf
    it "reads a sitemap" $ do
      let expected =
            [ Resource
                "abc"
                Internal
                "/index.html"
                "/index.html"
                [("Content-Type", "text/html")]
            ]
      eitherDecode
        "[[\"abc\",\"internal\",\"/index.html\",\"/index.html\",[[\"Content-Type\",\"text/html\"]]]]"
        `shouldBe` Right expected
      decode (encode expected) `shouldBe` Just expected
    it "validates application names" $ do
      isValidInstanceName "demo.example-1_x" `shouldBe` True
      isValidInstanceName "_" `shouldBe` False
      isValidInstanceName ".hidden" `shouldBe` False
      isValidInstanceName "../etc" `shouldBe` False
      isValidInstanceName "a/b" `shouldBe` False
      isValidInstanceName "" `shouldBe` False

  describe "Backup.Form" $ do
    it "creates an application from the backup identifier" $
      fmap
        (\(name, conf) -> (name, confAuthToken conf, confBackupDays conf))
        (newInstanceFromForm Tuesday [("full-token", "demo/secret")])
        `shouldBe` Right ("demo", "secret", [Tuesday])
    it "rejects an incorrect identifier" $ do
      fmap fst (newInstanceFromForm Tuesday [("full-token", "../x/y")]) `shouldBe` Left InvalidName
      fmap fst (newInstanceFromForm Tuesday [("full-token", "demo")]) `shouldBe` Left MalformedToken
      fmap fst (newInstanceFromForm Tuesday [("full-token", " ")]) `shouldBe` Left MissingToken
      fmap fst (newInstanceFromForm Tuesday []) `shouldBe` Left MissingToken
    it "updates the settings" $ do
      let form =
            [ ("backup-day", "Monday")
            , ("backup-day", "Monday")
            , ("backup-day", "Sunday")
            , ("history-size", "3")
            , ("proxy-host", "p.example.com")
            , ("proxy-port", "3128")
            ]
          conf = updateConfFromForm form sampleConf
      confBackupDays conf `shouldBe` [Monday, Sunday]
      confHistorySize conf `shouldBe` 3
      confProxy conf `shouldBe` Just (ProxyConf "p.example.com" 3128)
    it "keeps the previous history size when the new one is zero or invalid" $ do
      confHistorySize (updateConfFromForm [("history-size", "0")] sampleConf) `shouldBe` 5
      confHistorySize (updateConfFromForm [("history-size", "x")] sampleConf) `shouldBe` 5
    it "removes the proxy when one of its fields is empty" $
      confProxy (updateConfFromForm [("proxy-host", "p"), ("proxy-port", "")] sampleConf)
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
      map (parseLang . langCode) [minBound .. maxBound] `shouldBe` map Just [English, French]
    it "rejects an unknown language code" $
      parseLang "de" `shouldBe` Nothing
    it "formats the date of a snapshot in each language" $ do
      let t = localAt 2026 10 6 13 59 3
      render English (MsgSnapshotDate t) `shouldBe` "2026-10-06 13:59"
      render French (MsgSnapshotDate t) `shouldBe` "le 06-10-2026 à 13:59"
    it "translates messages with parameters" $ do
      render English (MsgBackupsOf "demo") `shouldBe` "Backups of demo"
      render French (MsgBackupsOf "demo") `shouldBe` "Sauvegardes de demo"
      render French (MsgDay Monday) `shouldBe` "Lundi"
    it "has a different text in each language for every day" $
      mapM_
        (\day -> render English (MsgDay day) `shouldNotBe` render French (MsgDay day))
        allDays
