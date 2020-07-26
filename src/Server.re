let app = Express.express();
let makeSuccessJson = () => {
  let json = Js.Dict.empty();
  Js.Dict.set(json, "success", Js.Json.boolean(true));
  Js.Json.object_(json);
};


[@bs.deriving jsConverter]
type webhookPayload = {
  text: string
};

let webHookUrl = "fillmein";
let httpPost = (url, data, thenF, catchF) =>
  Js.Promise.(
    Axios.postData(url, data)
    |> then_((response) => resolve(thenF(response##data)))
    |> catch((error) => { resolve(catchF(error))})
  );

let postPayload = (payload) => {
  let success = (_) => Js.log("Post Success");
  let fail = (_) => Js.log("Failed...");
  httpPost(webHookUrl, payload, success, fail);
}


Express.App.get(app, ~path="/", Express.Middleware.from((_, _) => {
  ignore(
    Js.Promise.(
      Bugsnag.main((result) => {
        {text: result}
        |> webhookPayloadToJs
        |> postPayload
        |> then_(() => resolve(Express.Response.sendJson(makeSuccessJson())))
      })
    )
  );
    Express.Response.end_;
}));

let onListen = _ => {()};

let port = 1337;
Express.App.listen(app, ~port=port, ~onListen, ());
Js.log("listening on " ++ string_of_int(port));
