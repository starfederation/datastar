# Datastar Java SDK

This package provides a Java SDK for working with [Datastar](https://data-star.dev/).

## License

This package is licensed for free under the MIT License.

## Requirements

This package requires Java 17 or later.

## Installation

Install using Maven by adding the following to your `pom.xml`:

```xml
<dependency>
    <groupId>com.starfederation</groupId>
    <artifactId>datastar</artifactId>
    <version>1.0.0</version>
</dependency>

<repositories>
    <repository>
        <id>github</id>
        <url>https://maven.pkg.github.com/starfederation/datastar</url>
    </repository>
</repositories>
```

## Usage

### Import Statements

```java

import StarFederation.Datastar.ServerSentEventGenerator;
```

### Set up Server-Sent Events Generator

```java
AbstractResponseAdapter responseAdapter = new HttpServletResponseAdapter(response);
ServerSentEventGenerator sse = new ServerSentEventGenerator(responseAdapter);
```

### Explanation of Events

```java
// This will create the event options based on the type.
MergeFragmentsOptions mergeFragmentsOptions = MergeFragmentsOptions.create();

// This will create the event based on the type and options.
MergeFragments mergeFragments = new MergeFragments(fragment, mergeFragmentsOptions);

// This will set the SSE spec options.
mergeFragments.setEventId(UUID.randomUUID().toString());
mergeFragments.setRetryDuration(1000);

// This will send the event.
sse.MergeFragments(mergeFragments);
```

### Merging HTML Fragments into the DOM

```java
String fragment = String.format("<div id='foo'>bar</div>");

MergeFragmentsOptions mergeFragmentsOptions = MergeFragmentsOptions.create();

MergeFragments mergeFragments = new MergeFragments(fragment, mergeFragmentsOptions);
mergeFragments.setEventId(UUID.randomUUID().toString());
mergeFragments.setRetryDuration(1000);

sse.MergeFragments(mergeFragments);
```

### Removing HTML Fragments from the DOM

```java
String selector = "#target";

RemoveFragmentsOptions removeFragmentsOptions = RemoveFragmentsOptions.create();

removeFragmentsOptions.withSettleDuration(20);
RemoveFragments removeFragments = new RemoveFragments(selector, removeFragmentsOptions);
removeFragments.setEventId(UUID.randomUUID().toString());

sse.RemoveFragments(removeFragments);
```

### Merging Signals

```java
DataStore store = new DataStore();
SignalReader.readSignals(adapter, store.getStore());

String input = store.get("input", String.class);
store.put("output", input);

MergeSignalsOptions mergeSignalsOptions = MergeSignalsOptions.create();
MergeSignals mergeSignals = new MergeSignals(store.toJson(), mergeSignalsOptions);
mergeSignals.setEventId(UUID.randomUUID().toString());
mergeSignals.setRetryDuration(1000);

sse.MergeSignals(mergeSignals);
```

### Removing Signals

```java
List<String> paths = Arrays.asList("user.name", "user.email");
RemoveSignals removeSignals = new RemoveSignals(paths);
removeSignals.setEventId(UUID.randomUUID().toString());

sse.RemoveSignals(removeSignals);
```

### Executing Scripts

```java
String script = "console.log('Hello from Datastar!')";

ExecuteScriptOptions executeScriptOptions = ExecuteScriptOptions.create()
    .withAutoRemove(true)
    .withAttributes("type module");
ExecuteScript executeScript = new ExecuteScript(script, executeScriptOptions);
executeScript.setEventId(UUID.randomUUID().toString());
executeScript.setRetryDuration(1000);

sse.ExecuteScript(executeScript);
```

### Reading Signals

```java
RequestAdapter adapter = new HttpServletRequestAdapter(request);

DataStore store = new DataStore();
SignalReader.readSignals(adapter, store.getStore());

String input = store.get("input", String.class);
store.put("output", input);
```
