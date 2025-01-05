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

import starfederation.datastar.utils.ServerSentEventGenerator;
```

### Set up Server-Sent Events Generator

You will want to set up the ServerSentEventGenerator to be able to handle the framework you choose to use. Adapters are provided for various framework requests and responses in the SDK.

```java
AbstractResponseAdapter responseAdapter = new HttpServletResponseAdapter(response);

ServerSentEventGenerator generator = new ServerSentEventGenerator(responseAdapter);
```

Once the generator is set up you can use it to send events. Let's utilize this event for the next few examples.

```java
MergeFragments event = MergeFragments.builder()
        .selector("#test")
        .data("<div>test</div>")
        .build();
```

This is how you would send an event with default [HTTP/SSE options](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events). The default options include a `monotonically increasing integer counter` for event ids and a retry duration of `1000ms`.

```java
generator.send(event);
```

If you want to modify these you can call send and pass the arguments you want for each accordingly.

```java
// custom event id
generator.send(event, "custom-id");

// custom retry duration
generator.send(event, 2000);

// both custom
generator.send(event, "custom-id", 2000);
```

### Events, Event Options, Examples

There are a few different event types in Datastar and each come with their own options. You can check out the [SDK Guide](https://github.com/rphumulock/datastar/blob/develop/sdk/README.md) for a review of all of them.

#### Example: Merging Fragments into the DOM

##### Options

- `selector` (string) The CSS selector to use to insert the fragments. If not provided or empty, Datastar **will** default to using the `id` attribute of the fragment.
- `mergeMode` (FragmentMergeMode) The mode to use when merging the fragment into the DOM. If not provided the Datastar client side **_will_** default to `morph`.
- `settleDuration` is used to control the amount of time that a fragment should take before removing any CSS related to settling. It is used to allow for animations in the browser via the Datastar client. If provided the value **_must_** be a positive integer of the number of milliseconds to allow for settling. If none is provided, the default value of `300` milliseconds will be used.
- `useViewTransition` Whether to use view transitions, if not provided the Datastar client side **_will_** default to `false`.

```java
MergeFragments event = MergeFragments.builder()
        .selector("#feed")
        .mergeMode(FragmentMergeMode.Append)
        .settleDuration(500)
        .useViewTransition(true)
        .data("<div id=\"feed\">\n<span>1</span>\n</div>")
        .build();
```

#### Example: Removing HTML Fragments from the DOM

##### Options

- `selector` (string) The CSS selector to use to insert the fragments. If not provided or empty, Datastar **will** default to using the `id` attribute of the fragment.
- `settleDuration` is used to control the amount of time that a fragment should take before removing any CSS related to settling. It is used to allow for animations in the browser via the Datastar client. If provided the value **_must_** be a positive integer of the number of milliseconds to allow for settling. If none is provided, the default value of `300` milliseconds will be used.
- `useViewTransition` Whether to use view transitions, if not provided the Datastar client side **_will_** default to `false`.

```java
RemoveFragments event = RemoveFragments.builder()
        .selector("#feed")
        .settleDuration(500)
        .useViewTransition(true)
        .build();
```

#### Example: Merging Signals

###### Options

- `onlyIfMissing` (boolean) Whether to merge the signal only if it does not already exist. If not provided, the Datastar client side will default to false, which will cause the data to be merged into the signals.

```java
MergeSignals event = MergeSignals.builder()
        .data("{\"key\": \"value\"}")
        .onlyIfMissing(true)
        .build();
```

#### Example: Removing Signals

```java
RemoveSignals event = RemoveSignals.builder()
        .addPath("user.name")
        .addPath("user.email")
        .build();
```

#### Example: Executing Scripts

###### Options

- `autoRemove` Whether to remove the script after execution, if not provided the Datastar client side will default to true.
- `attributes` A line separated list of attributes to add to the script element, if not provided the Datastar client side will default to type module. Each item in the array should be a string in the format key value.

```java
ExecuteScript event = ExecuteScript.builder()
        .script("console.log('Hello World');")
        .autoRemove(false)
        .attributes("type='module'")
        .build();

```

#### Example: Reading Signals

Often you would want to have a Datastore created on your backend to keep track of any signals in Datastar. Here is an example of how you would create a DataStore as well as read signals from a request.

```java
DataStore store = new DataStore();
SignalReader.readSignals(request, store);
```
