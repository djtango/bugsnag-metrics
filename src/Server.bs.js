// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Axios = require("axios");
var Express = require("bs-express/src/Express.js");
var Bugsnag$BugsnagMetrics = require("./Bugsnag.bs.js");

var app = Express.express(/* () */0);

function makeSuccessJson(param) {
  var json = { };
  json["success"] = true;
  return json;
}

function webhookPayloadToJs(param) {
  return {
          text: param[/* text */0]
        };
}

function webhookPayloadFromJs(param) {
  return /* record */[/* text */param.text];
}

var webHookUrl = "fillmein";

function httpPost(url, data, thenF, catchF) {
  return Axios.post(url, data).then((function (response) {
                  return Promise.resolve(Curry._1(thenF, response.data));
                })).catch((function (error) {
                return Promise.resolve(Curry._1(catchF, error));
              }));
}

function postPayload(payload) {
  var success = function (param) {
    console.log("Post Success");
    return /* () */0;
  };
  var fail = function (param) {
    console.log("Failed...");
    return /* () */0;
  };
  return httpPost(webHookUrl, payload, success, fail);
}

Express.App.get(app, "/", Express.Middleware.from((function (param, param$1) {
            Bugsnag$BugsnagMetrics.main((function (result) {
                    return postPayload(webhookPayloadToJs(/* record */[/* text */result])).then((function (param) {
                                  var partial_arg = makeSuccessJson(/* () */0);
                                  var partial_arg$1 = Express.$$Response.sendJson;
                                  return Promise.resolve((function (param) {
                                                return partial_arg$1(partial_arg, param);
                                              }));
                                }));
                  }));
            return Express.$$Response.end_;
          })));

function onListen(param) {
  return /* () */0;
}

Express.App.listen(app, 1337, onListen, /* () */0);

console.log("listening on " + String(1337));

var port = 1337;

exports.app = app;
exports.makeSuccessJson = makeSuccessJson;
exports.webhookPayloadToJs = webhookPayloadToJs;
exports.webhookPayloadFromJs = webhookPayloadFromJs;
exports.webHookUrl = webHookUrl;
exports.httpPost = httpPost;
exports.postPayload = postPayload;
exports.onListen = onListen;
exports.port = port;
/* app Not a pure module */
