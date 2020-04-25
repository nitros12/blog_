+++
title = "Writing our own event loops in python"
author = ["Ben Simms"]
date = 2019-03-20
tags = ["python", "programming"]
draft = false
+++

One of the good features of python is it's native support for coroutines,
generalised functions that can be paused and resumed, allowing objects to be
passed between the coroutine and whoever is running them when at the time.

The most widely used place for Python's coroutines currently is in asyncio
frameworks, namely the standard library module: AsyncIO, [Trio](https://github.com/python-trio/trio), and [Curio](https://github.com/dabeaz/curio). These
use python coroutines to allow the user to write code that requires IO resources
that may not be available at some point when their code runs, but will be in the
future. Without coroutine functions the alternative to this is promises, where
an IO request has a function attached that will run when the IO has completed,
however this approach leads to code becoming deeply nested and hard to work
with.

Coroutines fix this by allowing the function to be paused whenever an IO request
is made, the writer of the coroutine doesn't have to know that the function will
be paused and can write their function as if the IO resource magically becomes
available as soon as requested, they only need to insert the extra \`await\`
keyword to retrieve the results of other coroutines.

An example of a 'blocking' non async function:

```python
def make_request(url):
    result = my_http_lib.get(url)
    return result["data"]["some_value"]
```

This could be rewritten using promises to get:

```python
def make_request(url):
    return my_http_lib.get(url).then(lambda r: r["data"]["some_value"])
```

However that leads to messy code happening quickly, instead async functions the
following could be written:

```python
async def make_request(url):
    result = await my_async_http_lib.get(url)
    return result["data"]["some_value"]
```

Which as you can see, is practically identical in structure to the blocking version.

Asynchronous IO isn't the only problem that can be solved using coroutines, I am
now going to show how you can use coroutines to write a program that can resolve
references written in any order.

First, we need an object to signal actions to the event loop:

```python
from enum import Enum, auto

class RequestType(Enum):
    set_var = auto()
    get_var = auto()
```

We'll use tuples of a request and arguemtns to signal to the event loop what we
want performing.

Then we need an event loop

```python
from types import coroutine
from dataclasses import dataclass


@dataclass
class EventObject:
    coro: coroutine  # the running coroutine
    response: object = None  # the value to send to the object


class ELoop:
    def __init__(self):
        self.waiting = {}
        self.queue = []
        self.namespace = {}

    def sleep(self, obj, name):
        # multiple coros may wait on the same name, so store in a list
        also_waiting = self.waiting.setdefault(name, [])
        also_waiting.append(obj)

    def run_once(self):
        obj = self.queue.pop()

        while True:
            try:
                task, *args = obj.coro.send(obj.response)
                obj.response = None  # reset to none after sending
            except StopIteration:
                return

            if task is RequestType.set_var:
                name, value = args
                self.namespace[name] = value
            elif task is RequestType.get_var:
                # see if we know the name, if we do: set the value to send and
                # continue
                name = args[0]
                if name in self.namespace:
                    obj.response = self.namespace[name]
                    continue

                # if we reached here, the name isn't known yet, put the coro to sleep
                self.sleep(obj, name)
                return
            else:
                # throw an exception to the coroutine
                obj.coro.throw(ValueError("Invalid value yielded to loop"))

    def process(self, procs):
        # add processes to our queue
        self.queue.extend(EventObject(i) for i in procs)

        while self.queue:
            self.run_once()
            # after running once, we should scan and
            # see if any names are now known
            for name in tuple(self.waiting.keys()):
                if name in self.namespace:
                    response = self.namespace[name]

                    # this name is now known, wake up any coros that were
                    # waiting on it and add them to the queue
                    woke_objs = self.waiting.pop(name)

                    for obj in woke_objs:
                        obj.response = response

                    self.queue.extend(woke_objs)
```

And now we can write some functions that encode a computation of setting or
getting a variable:

```python
from types import coroutine

@coroutine
def set_var(name, value):
    yield RequestType.set_var, name, value


@coroutine
def get_var(name):
    return (yield RequestType.get_var, name)
```

Now we can write some programs:

```python
async def do_some_math():
    await set_var("one", 1)
    await set_var("two", 2)
    three = await get_var("three")
    four = await get_var("four")
    seven = three + four
    await set_var("seven", seven)
    print("Done some math")

async def do_some_more_math():
    one = await get_var("one")
    two = await get_var("two")
    three = one + two
    four = three + one
    await set_var("three", three)
    await set_var("four", four)
    seven = await get_var("seven")
    print(f"Done some more math, ended with: {seven}")
```

To run these we would do the following:

```python
loop = ELoop()

procs = [do_some_math(), do_some_more_math()]

loop.process(procs)
```

Running these gives the result:

```python
In [17]: loop.process(procs)
Done some math
Done some more math, ended with: 7
```

Pretty neat, huh?
