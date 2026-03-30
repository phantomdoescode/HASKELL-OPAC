module App.Error
  ( AppError(..)
  ) where

data AppError
  = NotFound String
  | AlreadyExists String
  | AuthError String
  | DBError String
  | ValidationError String
  deriving (Show, Eq)