# conduino

A lightweight continuation-based stream processing library.

It is similar in nature to pipes and conduit, but useful if you just want
something quick to manage stream processing.  There aren't much facilities for
IO --- you can implement IO stream processing if you want on top of the core
functionality.

## Why a stream processing library?

A stream processing library is a way to stream processors in a *composable* way:
instead of defining your entire stream processing function as a single
recursive loop with some global state, instead think about each "stage" of the process,
and isolate each state to its own segment.  Each component can contain its own
isolated state:

```haskell
runPipePure $ sourceList [1..10]
           .| scan (+) 0
           .| sinkList
-- [1,3,6,10,15,21,28,36,45,55]
```

All of these components have internal "state":

*   `sourceList` keeps track of "which" item in the list to yield next
*   `scan` keeps track of the current running sum
*   `sinkList` keeps track of all items that have been seen so far, as a list

They all work together without knowing any other component's internal state, so
you can write your total streaming function without concerning yourself, at
each stage, with the entire part.

In addition, there are useful functions to "combine" stream processors:

*   `zipSink` combines sinks in an "and" sort of way: combine two sinks in
    parallel and finish when all finish.
*   `altSink` combines sinks in an "or" sort of way: combine two sinks in
    parallel and finish when any of them finish
*   `zipSource` combines sources in parallel and collate their outputs.

Stream processing libraries are also useful for streaming composition of
monadic effects (like IO or State), as well.

## Details and usage

API-wise, is closer to *conduit* than *pipes*.  Pull-based, where the main
"running" function is:

```haskell
runPipe :: Pipe () Void u m a -> m a
```

That is, the "production" and "consumption" is integrated into one single
pipe, and then run all at once.  Contrast this to *pipes*, where
consumption is not integrated into the pipe, but rather your choice of
"runner" determines how your pipe is consumed.

One extra advantage over *conduit* is that we have the ability to model pipes
that will never stop producing output, so we can have an `await` function that
can reliably fetch items upstream.  This matches more *pipes*-style requests.

For a `Pipe i o u m a`:

*   `i` is the type of the upstream input expected
*   `o` is the type of the downstream output produced
*   `u` is the *result* type of the upstream pipe (the value it will see
    when the upstream pipe stops outputting)
*   `m` is the underlying monad where the actions take place
*   `a` is the result type that the pipe will produce once it 

From these, we can specialize:

*   If `i` is `()`, then the pipe is a *source*: it doesn't expect any input to
    be able to pump out items.

    If `a` for a source is `Void`, it means that the source will produce values
    forever.
*   If `o` is `Void`, then the pipe is a *sink*: it won't ever output any
    items downstream.
*   If if a pipe is both a source and a sink (`Pipe () Void`), the pipe is an
    *effect*: It is something that can be run as an effect with `runPipe`.

Normally, we have `await`:

```haskell
await :: Pipe i o u m (Maybe i)
```

It will return `Nothing` if the upstream pipe stops producing.

However, if `u` is `Void`, we can use:

```haskell
awaitSurely :: Pipe i o Void m i
```

for an `await` that will *always* return a value.  That is because `u` is the
type of the upstream pipe's result, but if `u` is `Void`, it means that the
upstream pipe will never finish with a result, and keep producing forever.

## Why not pipes or conduit?

*conduino* is a small, lightweight version that is focused not necessarily on
"effects" streaming, but rather on composable bits of logic.  It is basically a
lightweight version of conduit-style streaming.  It is slightly different from
pipes in terms of API.

One major difference from *conduit* is the `u` parameter, which allows for
things like `awaitSurely`, to ensure that upstream pipes will never terminate.

If you need to do some important IO and handle things like managing resources,
or leverage interoperability with existing libraries...switch to a more mature
library like *conduit* or *pipes* immediately :)
