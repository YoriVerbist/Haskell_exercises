module Template where

data Attr = Attr String String 
    deriving (Eq, Show)

data HtmlElement = HtmlString String
    | HtmlTag String [Attr] HtmlElements
    deriving (Eq, Show)

type HtmlElements = [HtmlElement]

class HTML a where
    toHtml :: a -> HtmlElement

data Link = Link {link :: String, text:: String}

instance HTML Link where
    toHtml (Link link text) = HtmlTag "a" [Attr "href" link] [HtmlString text]

exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" [] [HtmlTag "li" [] [HtmlString "Apples"],
                            HtmlTag "li" [] [HtmlString "Bananas"],
                            HtmlTag "li" [] [HtmlString "Oranges"]]

instance HTML a => HTML [a] where
    toHtml l = HtmlTag "ul" [] $ map (\x -> HtmlTag "li" [] [toHtml x]) l
