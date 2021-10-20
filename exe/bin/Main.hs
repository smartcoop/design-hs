module Main
  ( main
  ) where

import qualified Conf
import qualified Conf.Parse                    as CP
import qualified Conf.Types                    as CT
import qualified Data.Text.IO                  as T
import qualified Options.Applicative           as A
import           Smart.Html.Render             as R
import           System.FilePath.Posix          ( (</>) )

-- | Parse the configuration from the cli and run.
main :: IO ExitCode
main = A.execParser CP.confParserInfo >>= mainWithConf

mainWithConf :: CT.Conf -> IO ExitCode
mainWithConf cnf@CT.Conf {..} = do
  Conf.scaffoldFilesystem cnf mempty

  let indexHtml = R.renderCanvasWithHeadText undefined

  T.writeFile indexF indexHtml
  undefined
  where indexF = _cOutputDir </> "index.html"
