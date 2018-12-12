module Samples exposing (tourDocumentSources)

import Dict exposing (Dict)
import Samples.API
import Samples.Article
import Samples.Button
import Samples.Clock
import Samples.Csv
import Samples.FileBrowserModel
import Samples.Images
import Samples.MessagingSchema
import Samples.TypographyEssentials
import Samples.UserProfile
import Samples.Welcome
import Samples.WikiModel


tourDocumentSources : Dict String String
tourDocumentSources =
    [ ( "01-welcome", Samples.Welcome.source )
    , ( "02-clock", Samples.Clock.source )
    , ( "03-button", Samples.Button.source )
    , ( "04-images", Samples.Images.source )
    , ( "05-csv", Samples.Csv.source )
    , ( "06-api", Samples.API.source )
    , ( "07-user-profile", Samples.UserProfile.source )
    , ( "08-wiki-model", Samples.WikiModel.source )
    , ( "09-typography-essentials", Samples.TypographyEssentials.source )
    , ( "10-file-browser", Samples.FileBrowserModel.source )
    , ( "11-messaging-schema", Samples.MessagingSchema.source )
    , ( "12-article", Samples.Article.source )
    , ( "now-you", "# Now your turn!" )
    ]
        |> Dict.fromList
