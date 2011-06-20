{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Mode where

data Mode =
  ModeStatic |
  ModeDynamic
  deriving (Show, Eq, Ord)

xmlMode :: Mode -> String
xmlMode ModeStatic = "static"
xmlMode ModeDynamic = "dynamic"