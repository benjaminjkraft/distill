{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Applicative
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Clock
import Text.XML as XML

import Language.Google.Search.Simple
import Language.Google.Search.Mail
import Data.Google.Mail.Filters

deriving instance Eq t => Eq (Term t)
deriving instance Eq t => Eq (BooleanF t)
deriving instance Eq Location
deriving instance Eq Duration
deriving instance Eq Status
deriving instance Eq Feature
deriving instance Eq Size
deriving instance Eq Category
deriving instance Eq MailOp
deriving instance Eq Action
deriving instance Ord t => Ord (Term t)
deriving instance Ord t => Ord (BooleanF t)
deriving instance Ord Location
deriving instance Ord Duration
deriving instance Ord Status
deriving instance Ord Feature
deriving instance Ord Size
deriving instance Ord Category
deriving instance Ord MailOp
instance Ord Action where
    --we don't derive the instance because it is convenient later on to
    --have things that need deferencing at the end.
    compare Archive Archive = EQ
    compare Archive _ = LT
    compare _ Archive = GT
    compare Delete Delete = EQ
    compare Delete _ = LT
    compare _ Delete = GT
    compare MarkAsRead MarkAsRead = EQ
    compare MarkAsRead _ = LT
    compare _ MarkAsRead = GT
    compare NeverSpam NeverSpam = EQ
    compare NeverSpam _ = LT
    compare _ NeverSpam = GT
    compare Star Star = EQ
    compare Star _ = LT
    compare _ Star = GT
    compare (MarkAsImportant a) (MarkAsImportant b) = compare a b
    compare (MarkAsImportant _) _ = LT
    compare _ (MarkAsImportant _) = GT
    compare (Categorise a) (Categorise b) = compare a b
    compare (Categorise _) _ = LT
    compare _ (Categorise _) = GT
    compare (ForwardTo a) (ForwardTo b) = compare a b
    compare (ForwardTo _) _ = LT
    compare _ (ForwardTo _) = GT
    compare (LabelAs a) (LabelAs b) = compare a b
    -- compare (LabelAs _) _ = LT
    -- compare _ (LabelAs _) = GT


renderFilters :: (Maybe FilePath) -> T.Text -> T.Text -> [Filter] -> IO ()
renderFilters path name email filters = do
    now <- getCurrentTime
    let writer = maybe TL.putStrLn TL.writeFile path
    writer . XML.renderText def {rsPretty = True} $
        toXML now (name, email) filters

deferenceFilter :: Filter -> [Filter]
deferenceFilter (Filter actions search) = [Filter action search | action <- deferenceActions actions] --may create one more filter than necessary

deferenceActions :: [Action] -> [[Action]]
deferenceActions = (split $ keepDelimsR $ dropInitBlank $ dropFinalBlank $ whenElt needsDeferencing) . sort

needsDeferencing :: Action -> Bool
needsDeferencing (LabelAs _) = True
needsDeferencing (ForwardTo _) = True
needsDeferencing (Categorise _) = True
needsDeferencing _ = False

includeParents :: Filter -> Filter
includeParents (Filter actions search) = Filter (concatMap includeParentsAction actions) search

includeParentsAction :: Action -> [Action]
includeParentsAction (LabelAs path) = map LabelAs $ parents path
includeParehtsAction action = action

parents :: T.Text -> [T.Text]
parents path = map (T.intercalate "/") $ tail $ inits $ T.split (=='/') path

masterFilter :: Simple -> T.Text -> [Mail] -> [Filter]
masterFilter listname path excludes = deferenceFilter $ includeParents $ Filter [LabelAs path] search
  where search = if null excludes
                   then pure $ To listname
                   else (pure $ To listname) /\ (notB $ orB excludes) -- uugh, orB [] errors...
