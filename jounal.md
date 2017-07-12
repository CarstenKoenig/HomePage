# developers journal

## 12.06.

### working out login/logout

So it seems that *auth* is not really easy in Servant, neither is *session handling*.

Luckily I've found a nice tutorial [here](https://www.stackbuilders.com/tutorials/haskell/servant-auth/)
let's see how this works out.

---

#### there be problems
While transcriping the tutorial I stumpled upon this error:

  . Couldn't match type '(->) Request'
                   with 'Control.Monad.Trans.Except.ExceptT ServantErr IO'
				   
happening where I had a `serveDirectory :<|> return ...` that I transformed using `enter`

Turns out that `serveDirectory` will not really play well with `enter` here because the
`ServerT` for the instance for `HasServer Raw` is `Application` (which is a `Request -> ...`)

So better seperate those parts of the API!

---

#### session trouble
I had some trouble finding a way to redirect and login/logoff as the only way
to redirect I found was throwing errors around

In the end the clue was to create an error with `addSessionToErr` and then
throw that.
