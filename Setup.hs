import           Distribution.PackageDescription
import           Distribution.Simple
import           Language.Java.Inline.Cabal      (gradleHooks,
                                                  prependClasspathWithGradle)
import           System.Environment

main = defaultMainWithHooks (customHooks (gradleHooks simpleUserHooks))

customHooks hooks =
  hooks
  { preRepl = addBuildClassPath <> prependClasspathWithGradle <> preRepl hooks
  , preBuild = addBuildClassPath <> preBuild hooks
  }


addBuildClassPath :: Args -> b -> IO HookedBuildInfo
addBuildClassPath _ _ = do
  original <- lookupEnv "CLASSPATH"
  let buildClasses = "build/libs/classpath.jar"
  setEnv "CLASSPATH" (maybe buildClasses (buildClasses ++) original)
  return (Nothing, [])
