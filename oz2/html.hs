module Template where

-- * HTML
-- ----------------------------------------------------------------------------

-- Simple (X)HTML markup.
data Attr = MkAttr String String
  deriving (Eq,Show)

data HtmlElement
  = HtmlString String                    -- Plain text.
  | HtmlTag String [Attr] HtmlElements   -- Structured markup.
  deriving (Eq, Show)

type HtmlElements = [HtmlElement]

example :: HtmlElement
example =
  HtmlTag "a" [MkAttr "href" "https://www.kuleuven.be/kuleuven/"]
    [HtmlString "KU Leuven"]

-- HTML renderable class.
class HTML a where
  toHtml :: a -> HtmlElement

data Link =
  Link
    String  -- Link target.
    String  -- Text to show.
  deriving (Eq,Show)

instance HTML Link where
  toHtml (Link link text) = HtmlTag "a" [MkAttr "href" link] [HtmlString text]

-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Apples</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>
exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" [] [
    HtmlTag "li" [] [HtmlString "Apples"],
    HtmlTag "li" [] [HtmlString "Bananas"],
    HtmlTag "li" [] [HtmlString "Oranges"]
    ]

instance HTML String where
    toHtml str = HtmlString str

instance HTML a => HTML [a] where
   toHtml l = HtmlTag "ul" [] $ map parseToLi l
        where 
            parseToLi x = HtmlTag "li" [] [toHtml x]
        

data AddressBook = DEFINE_ADDRESSBOOK

myAddressBook :: AddressBook
myAddressBook = error "Not implemented"

instance HTML AddressBook where
  toHtml = error "Not implemented"


printHtmlString :: HtmlElement -> IO ()
printHtmlString = putStrLn . toHtmlString

toHtmlString :: HtmlElement -> String
toHtmlString (HtmlString s) = s ++ "<br>"
toHtmlString (HtmlTag t attrs els) =  unlines (openTag : map toHtmlString els) ++ closeTag
  where
    openTag  = '<' : t ++ " " ++ unwords (map showAttr attrs) ++ ">"
    closeTag = '<' : '/' : t  ++ ">"
    showAttr (MkAttr name value) = unwords [name, "=", '\"':value ++ "\""]

