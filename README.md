# The Shopping List Problem


<p align="center">
  <img width="300" height="354" src="https://raw.githubusercontent.com/Far117/Shopping-List-Problem/master/graph.png">
</p>

The Shopping List problem is a problem inspired by a task to optimize a game strategy. The problem is as follows:

Assume you have a list of items you need to buy from a store. The store has several container types available to you. Each container type has a price and the *possibility* of having certain items in it. These probabilities are independent of each other. The goal is to figure out in what order you should buy the containers to satisfy your shopping list for the least cost, on average.

## Example

An example can make this much clearer:

You need to get one **X** and one **Y**.

Container **A** costs $10 and has a 1/2 chance of containing an **X**.

Container **B** costs $10 and has a 1/2 chance of containing a **Y**.

Container **C** costs $20 and has a 1/3 chance of containing an **X**, *and* a 1/3 chance of containing a **Y**.

Note that in the example of **C**, these probabilities are independent. As such, it isn't "a 1/3 chance of an **X** or **Y**." Rather, it's like rolling two three-sided dice. The container can have a single **X**, a single **Y**, both an **X** and a **Y**, or nothing at all.

## Further Reading

A fully comprehensive explanation of the solution for a generalized shopping list can be found in the PDF, and the included Haskell program employs the algorithm.
