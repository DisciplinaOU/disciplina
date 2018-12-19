
import Data.Traversable

-- import Data.Aeson
import Servant.Client

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Educator
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types
-- import Dscp.Witness.Web.Client

import Test.QuickCheck

main :: IO ()
main = do
    [port] <- getArgs

    wClient <- createEducatorApiClient =<< parseBaseUrl ("localhost:" ++ port)

    courses <- for [1..5 :: Int] $ \_ -> do
        eAddCourse wClient NewCourse
            { ncId = Nothing
            , ncDesc = fromString "Doing nothing"
            , ncSubjects = [Subject 1, Subject 2]
            }

    sk <- genSecretKey

    student <- eAddStudent wClient NewStudent
        { nsAddr = mkAddr (toPublic sk)
        }

    contentHash <- generate arbitrary

    asses <- for courses $ \course -> do
        () <- eAddAssignment wClient True NewAssignment
            { naCourseId = course
            , naContentsHash = contentHash
            , naIsFinal = IsFinal True
            , naDesc = fromString  $ "Chilling right there at " ++ show course
            }
        eGetAssignments wClient (Just course) Nothing Nothing Nothing False

    enrollments <- for courses $ \course -> do
        eAddStudentCourse wClient (mkAddr $ toPublic sk) (NewStudentCourse course)

    sClient <- createStudentApiClient =<< parseBaseUrl ("localhost:" ++ port)

    print =<< sGetAssignments sClient Nothing Nothing Nothing False

    print (student, asses, enrollments)

    -- sClient <- createStudentApiClient =<< parseBaseUrl "127.0.0.1:8090"
    -- asses   <- getAssignments sClient
    -- print asses
