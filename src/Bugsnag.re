let fillmein = "fillmein"
/* let orgId = fillmein; */
let orgId = "546ca5ab77656207ea00079a";
/* let authToken = fillmein; */
let authToken = "token f4cf4816-c52e-4290-9263-04ca6fa60cdd";
let headers = Axios.Headers.fromObj({"Authorization": authToken, "X-Version": "2"});

let bugsnag = "https://api.bugsnag.com/";

let organizations = bugsnag ++ "organizations/" ++ orgId;

type orgsResp = {
  projectsUrl: string
};

type projectEvent = {
  slug: string,
  errorsUrl: string
};

let isMyProject = (e) => {
  switch (e.slug) {
    | fillmein => true
    | _ => false
  };
};

type dateString = string;

// errorRespToJs
// errorRespFromJs
[@bs.deriving jsConverter]
type errorResp = {
  id: string,
  errorClass: string,
  events: int,
  firstSeen: dateString,
  lastSeen: dateString,
  firstSeenUnfiltered: dateString,
  lastSeenUnfiltered: dateString,
  status: string
};

module Decode = {
  let orgsResp = json =>
    Json.Decode.{
      projectsUrl: json |> field("projects_url", string)
    };
  let projectEvent = json =>
    Json.Decode.{
      slug: json |> field("slug", string),
      errorsUrl: json |> field("errors_url", string)
    };
  let projectEvents = projectEvent |> Json.Decode.array;
  let errorResp = json =>
    Json.Decode.{
      id: json |> field("id", string),
      errorClass: json |> field("error_class", string),
      events: json |> field("events", int),
      firstSeen: json |> field("first_seen", string),
      lastSeen: json |> field("last_seen", string),
      firstSeenUnfiltered: json |> field("first_seen_unfiltered", string),
      lastSeenUnfiltered: json |> field("last_seen_unfiltered", string),
      status: json |> field("status", string)
    };
  let errorResps = errorResp |> Json.Decode.array;
};

let httpGet = (url, thenF, catchF) => {
  ignore(Js.log("GET:" ++ url));

  Js.Promise.(
    Axios.getc(url, Axios.makeConfig(~headers, ()))
    |> then_((response) => { Js.log("success!"); resolve(thenF(response##data))})
    |> catch((error) => { resolve(catchF(error))})
    )
};

let retryAfter = (headers) => {
  let retryAfter = Js.Nullable.toOption(headers##"retry-after");
  switch (retryAfter) {
    | None => Js.log("no retry found waiting 5 seconds..."); 5
    | Some(s) => s
  }
};

external promiseErrorToJsObj : Js.Promise.error => Js.t('a) = "%identity";
let rec httpGetBatch = (url, params, thenF) => {
  Js.log("GET-batch:" ++ url);
  Js.Promise.(
    Axios.getc(url, Axios.makeConfig(~headers, ~params, ()))
    |> then_((response) => resolve(
      thenF(response##headers, response##data)
    ))
    |> catch((error) => {
      let retry = (e) => {
        if (e##response##status == 429) {
          // throttled
          Js.log("throttled...");
          let retryAfter = retryAfter(e##response##headers);
          Js.log("sleeping for " ++ string_of_int(retryAfter) ++ " seconds...");
          ignore(Js.Global.setTimeout(() => {
            ignore(httpGetBatch(url, params, thenF));
            ();
          },
          retryAfter * 1000));
          ();
        } else {
          Js.log("something went wrong...");
          Js.log(error);
          ();
        }
      };
      resolve(error |> promiseErrorToJsObj |> retry);
      })
  );
}

let httpError = (error) => {
  Js.log(error);
  "error occurred";
};

let getErrorsUrl = e => e.errorsUrl;

// time fallacies...
type time = int;

let _1ms: time = 1;
let _1s: time = 1000 * _1ms;
let _1min: time = 60 * _1s;
let _1h: time = 60 * _1min;
let _1d: time = 24 * _1h;
let _1w: time = 7 * _1d;

let transformTime = (f, dateString) => {
  let dF = Js.Date.parseAsFloat(dateString);
  let dF2 = f(dF);
  let date = Js.Date.fromFloat(dF2);
  Js.Date.toISOString(date);
};
let addTime = (dateString, t) => {
  transformTime(d => d +. float_of_int(t), dateString);
};
let oneWeekAgo = dateString => {
  addTime(dateString, -_1w);
};

let subtract1Ms = (dateString: dateString) => {
  let dF = Js.Date.parseAsFloat(dateString);
  let dF2 = dF -. 1.;
  let date = Js.Date.fromFloat(dF2);
  Js.Date.toISOString(date);
};

let isWithinLastWeek = (dateString, now) => {
  dateString >= oneWeekAgo(now);
};

let isFirstSeenWithinLastWeek = (errorResp) => {
  errorResp.firstSeen |> t => isWithinLastWeek(t, Js.Date.(now() |> fromFloat |> toISOString));
};

let makeParams = (base) => {
  Js.Dict.{
    "base": base,
    "per_page": 100,
    "filters[event.since][][type]": "eq",
    "filters[event.since][][value]": "7d"
  };
};

let nextPage = (data) => {
  let error = Array.get(data, Array.length(data) - 1);
  let earliestTime = error.lastSeen;
  makeParams(subtract1Ms(earliestTime));
};

let fetchAll = (url, doneF) => {
  let initData = [||];
  let initParams = Js.Date.now() |> Js.Date.fromFloat |> Js.Date.toISOString |> makeParams;
  let rec iter = (data, params) => {
    httpGetBatch(url, params, (headers, d) => {
      let newData = d |> Decode.errorResps;
      let moreData = Array.append(data, newData);
      let count = Array.length(moreData);
      let totalCount = headers##"x-total-count";
      if (totalCount > 0) {
        Js.log("paging");
        Js.log(totalCount);
        Js.log(count);
        ignore((iter(moreData, nextPage(moreData))));
        ();
      } else {
        Js.log("done");
        doneF(moreData);
        ();
      };
    });
  };
  iter(initData, initParams);
};

module StringId = {
  type t = string;
  let compare = (x0, x1) => Pervasives.compare(x0, x1);
};

module ErrorIdMap = Map.Make(StringId);

// INDIVIDUAL error level reporting
/* let errorId = fillmein; */
/* let groupByCount = (xs) => { */
/*   Array.fold_left((m, x) => { */
/*     let (id, events) = x; */
/*     if (id == errorId) { */
/*       Js.log("found thing: " ++ string_of_int(events)); */
/*     }; */
/*     let n = */
/*       try (ErrorIdMap.find(id, m)) { */
/*         | Not_found => 0 */
/*       }; */
/*     ErrorIdMap.add(id, n + 1, m); */
/*   }, */
/*   ErrorIdMap.empty, */
/*   xs) */
/* }; */
/*  */
/* let printEventsByErrorId = (data) => { */
/*   data */
/*     |> (xs) => { */
/*       Js.log("total count: " ++ string_of_int(Array.length(xs))); */
/*       Array.map((x) => (x.id, x.events), xs) */
/*     } */
/*   |> groupByCount */
/*   |> (m) => (ErrorIdMap.mapi((k, v) => { */
/*     if (v > 1) { */
/*       Js.log( k ++ ": " ++ string_of_int(v)); */
/*     }; */
/*     v; */
/*   }, m) |> ignore); */
/* }; */

let findFromLastWeek = errorResp =>
errorResp
  |> Array.to_list
  |> l => List.filter(isFirstSeenWithinLastWeek, l)
  |> Array.of_list;

let numberOfErrorsThisWeek = errorResps => {
  errorResps |> findFromLastWeek |> (x) => Array.map(errorRespToJs, x) |> Array.length |> string_of_int;
};

let percentageErrorsSnoozed = errorResps => {
  let isSnoozed = status => {
    switch (status) {
      | "snoozed" => true
      | _ => false
    };
  };
  let totalCount = Array.length(errorResps) -> float_of_int
  errorResps
    |> es => Array.map(e => e.status, es)
    |> Array.to_list
    |> l => List.filter(isSnoozed, l)
    |> List.length
    |> float_of_int
    |> snoozed => snoozed /. totalCount *. 100.0
    |> string_of_float
};

let printReport = d => {
  let errorsThisWeek = d |> numberOfErrorsThisWeek |> msg => ("New errors introduced this week: " ++ msg);
  let totalErrors = d |> Array.length |> string_of_int |> n => ("Total errors with events this week: " ++ n);
  let errorsSnoozed = d |> percentageErrorsSnoozed |> pc => ("% errors being snoozed: " ++ pc ++ "%");
  String.concat("\n", [errorsThisWeek, totalErrors, errorsSnoozed]);
};

let main = callback => {
  Js.Date.( now() |> fromFloat |> toISOString |> Js.log );
  Js.log("pinging bugsnag");
  Js.Promise.(
    httpGet(organizations,
            (data) => { Decode.orgsResp(data).projectsUrl },
            httpError)
    |> then_((projectsUrl) => {
      httpGet(projectsUrl,
              (data) => {
                Js.log(data);
                data |> Decode.projectEvents |> Array.to_list |> l=> List.filter(isMyProject, l);
                },
                (_) => [])
    })

    |> then_((projectErrorsUrls) => {
      resolve(List.map((url) => {
        let makeResult = data => {
          "Report for " ++ url.slug ++  ":\n" ++ printReport(data);
        };
        ignore(fetchAll(getErrorsUrl(url), x => x |> makeResult |> callback));
        url;
      }, projectErrorsUrls));
    })
  );
};
