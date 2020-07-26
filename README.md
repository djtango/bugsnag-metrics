# Bugsnag Metrics

This is _tiiiny_ service to run a report some high level metrics on your
bugsnag data. This is an example report:  

```
Report for importer:
New errors introduced this week: 2
Total errors with events this week: 5
% errors being snoozed: 40.%
```

This is intended to be used as a Slack command. If you register your endpoint
in the Slack interface you can register a webhook command that will ping this
server.

Note that the configuration variables `webHookUrl` `orgId` `authToken` and `isMyProject` are
hardcoded and need to be filled in  

Also note: don't put this in production.

# Build
You can build this locally using the command  
(If you need a Reason docker environment you can use
[mine](https://github.com/djtango/reason-alpine-docker))

```

npm run build
```

# Running the server

Is as simple as either:
```
npm run server
```

or  

```
node src/Server.bs.js
```
