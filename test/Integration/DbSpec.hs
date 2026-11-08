{-# LANGUAGE OverloadedStrings #-}

module Integration.DbSpec (spec) where

import qualified Data.ByteString.Char8 as BS8
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Test.Hspec
import Yaptape.Db.Postgres (getAppliedMigrations, runMigrations)

spec :: SpecWith Pool.Pool
spec = describe "Database Migrations & Constraints (Integration)" $ do
  it "records all applied migrations in lexicographical order and is idempotent" $ \pool -> do
    initial <- getAppliedMigrations pool
    initial `shouldBe` Right ["001_create_mixtapes.sql", "002_add_field_limits.sql"]

    rerunResult <- runMigrations pool
    rerunResult `shouldBe` Right ()

    afterRerun <- getAppliedMigrations pool
    afterRerun `shouldBe` Right ["001_create_mixtapes.sql", "002_add_field_limits.sql"]

  it "enforces PostgreSQL CHECK constraints on mixtapes and tracks tables" $ \pool -> do
    blankTitleRes <- Pool.use pool (Session.sql "INSERT INTO mixtapes (title) VALUES ('   ')")
    blankTitleRes `shouldSatisfy` isLeft

    let overlongTitle = BS8.pack ("INSERT INTO mixtapes (title) VALUES ('" ++ replicate 121 'a' ++ "')")
    overlongTitleRes <- Pool.use pool (Session.sql overlongTitle)
    overlongTitleRes `shouldSatisfy` isLeft

    let overlongDesc = BS8.pack ("INSERT INTO mixtapes (title, description) VALUES ('Valid', '" ++ replicate 501 'b' ++ "')")
    overlongDescRes <- Pool.use pool (Session.sql overlongDesc)
    overlongDescRes `shouldSatisfy` isLeft

    invalidVideoRes <- Pool.use pool $ Session.sql
      "WITH m AS (INSERT INTO mixtapes (title) VALUES ('Constraint Test') RETURNING id) \
      \INSERT INTO tracks (mixtape_id, track_order, video_id, note) \
      \SELECT id, 0, 'invalid-id', 'Valid note' FROM m"
    invalidVideoRes `shouldSatisfy` isLeft

    invalidOrderRes <- Pool.use pool $ Session.sql
      "WITH m AS (INSERT INTO mixtapes (title) VALUES ('Constraint Test') RETURNING id) \
      \INSERT INTO tracks (mixtape_id, track_order, video_id, note) \
      \SELECT id, 100, 'dQw4w9WgXcQ', 'Valid note' FROM m"
    invalidOrderRes `shouldSatisfy` isLeft

    blankNoteRes <- Pool.use pool $ Session.sql
      "WITH m AS (INSERT INTO mixtapes (title) VALUES ('Constraint Test') RETURNING id) \
      \INSERT INTO tracks (mixtape_id, track_order, video_id, note) \
      \SELECT id, 0, 'dQw4w9WgXcQ', '   ' FROM m"
    blankNoteRes `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
