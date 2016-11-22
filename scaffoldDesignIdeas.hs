-- Final Project: Brenda and Jamie! --

parseURL :: URL -> Webpage
  -- see if URL contains query. if not, pass empty query to interpretQuery
  -- if query, interpretQuery query

interpretQuery :: Query -> Webpage
  -- if text query == "", displayQueryForm
  -- else, displaySearchResults (search Query)
  -- TODO use Maybe instead of empty string

displayQueryForm :: Webpage
  -- shows a box and asks for input
  -- TODO have a function reacts once form is submitted
  -- we can use 'do' notation -- this bind to that, to that, to that

displaySearchResults :: Maybe [Tweet] -> Webpage

search :: Query -> Maybe [Tweet]
-- TODO: Brenda: this function

-- TODO: Jamie: match requests/URLs to templates/pages to serve
