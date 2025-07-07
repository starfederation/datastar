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
PatchElements event = PatchElements.builder()
        .selector("#test")
        .data("<div>test</div>")
        .build();
```

#### Sending Events

This is how you would send an event with default [HTTP/SSE options](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events). The default options include a `monotonically increasing integer counter` for event ids and a retry duration of `1000ms`.

```java
generator.send(event);
```

#### Custom Event IDs and Retry Durations
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

There are a few different event types in Datastar and each come with their own options. You can check out the [SDK Guide](https://github.com/starfederation/datastar/blob/develop/sdk/README.md) for a review of all of them.

#### Example: Patch Elements into the DOM

This event is used to patch elements into the DOM. The data is a string that represents the HTML to be patched into the DOM.

##### Options

- `selector` (string) The CSS selector to use to insert the element. If not provided or empty, Datastar **will** default to using the `id` attribute of the element.
- `mode` (ElementPatchMode) The mode to use when patching the element into the DOM. If not provided the Datastar client side **_will_** default to `outer`.
- `useViewTransition` Whether to use view transitions, if not provided the Datastar client side **_will_** default to `false`.

```java
PatchElements event = PatchElements.builder()
        .selector("#feed")
        .mode(ElementPatchMode.Append)
        .useViewTransition(true)
        .data("<div id=\"feed\">\n<span>1</span>\n</div>")
        .build();
```

#### Example: Removing HTML Elements from the DOM

This event is used to remove elements from the DOM.

##### Options

- `selector` (string) The CSS selector to use to remove the elements. If not provided or empty, Datastar **will** default to using the `id` attribute of the element.
- `useViewTransition` Whether to use view transitions, if not provided the Datastar client side **_will_** default to `false`.

```java
PatchElements event = PatchElements.builder()
        .selector("#feed")
        .mode(ElementPatchMode.Remove)
        .useViewTransition(true)
        .build();
```

#### Example: Patching/Updating Signals

This event is used to patch data into the signals object on the client side. The data is a JSON object that represents the data to be patched into the signals object.

###### Options

- `onlyIfMissing` (boolean) Whether to patch the signals only if they do not already exist. If not provided, the Datastar client side will default to false, which will cause the data to be patched into the signals.

```java
PatchSignals event = PatchSignals.builder()
        .data("{\"key\": \"value\"}")
        .onlyIfMissing(true)
        .build();
```

#### Example: Removing Signals

For a signal to be removed, the signal values must be set to `null`. The signals can be nested, and therefore to remove a leaf value, you have to provide the path to the leaf.

```java
PatchSignals event = PatchSignals.builder()
        .data("{\"user\": {\"name\":null, \"email\": null}}")
        .build();
```

#### Example: Executing Scripts

This event is used to execute a script on the client side. The script is a string that represents the script to be executed. The script can be any valid JavaScript code. The script will be executed in the context of the client side.

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

This will read signals from a request and merge them into the signals object on the backend. The request is an object that represents the request to read the signals from. The store is a DataStore object that represents the store to merge the signals into. 

Often you would want to have a Datastore created on your backend to keep track of any signals in Datastar. Here is an example of how you would create a DataStore as well as read signals from a request.

```java
DataStore store = new DataStore();
SignalReader.readSignals(request, store);
```
