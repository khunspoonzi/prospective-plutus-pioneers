# [Commit Schemes](https://youtu.be/uwZ903Zd0DU?t=30)

A commit scheme is a form of interaction between at least two entities, which allows one entity to commit to a decision without revealing that decision to the other entity, but in such a way that can be verified after the fact.

We'll demonstrate how this works in the context of a game between Alice and Bob.

## Example: [Alice and Bob Play a Game](https://youtu.be/uwZ903Zd0DU?t=30)

**Nonce:** An arbitrary bytestring that can be used for cryptographic computations

---

Suppose that Alice and Bob play a game whereby Alice wins if both Alice and Bob choose make the same choice given two possible options, and Bob wins otherwise. In this case, they can choose between 0 or 1:

```
                    Bob
               |    0    1
        ----------------------
Alice     0    |    A    B
          1    |    B    A
```

Let us further assume that Alice and Bob are not in the same timezone and must therefore play this game over email instead of in real-time, whereby Alice writes first and Bob responds.

A problem arises from this scenario because Bob will always know what Alice has chosen before he makes his decision. Bob can therefore gurantee that he wins every time by choosing the opposite of whatever Alice chooses.

Fortunately, the world of cryptography introduces a concept known as "commit schemes", which will allow Alice to commit to her choice without revealing it to Bob. This can be accomplished using hash functions:

```
hash(0) = zr56y1
hash(1) = ff21x0
```
In this case, Alice makes her choice and then hashes it before sending it over to Bob so that it isn't directly revealed to him beofre he makes his own choice and sends it (unhashed) to Alice.

If Alice has won, she can inform Bob of her choice, which he can then verify for himself by passing her choice into the same hash function and comparing the results.

Of course, the situation is still problematic if Alice and Bob play enough times for Bob to learn which hashes correspond to which options, or if Bob has access to the hash function upon receiving Alice's choice.

A solution to this would be for Alice to first concatenate an arbitrary bytestring called a "nonce" to her choice before passing it into the hash function and sending the result to Bob:

```
hash(nonce || 0) = hp88n3
hash(nonce || 1) = tw43k6
```

Assuming that Alice uses a different nonce each time they play the game, the result of passing her choice into the hash function will differ each time, regardless of whether she chooses the same option multiple times.

As before, Bob sends his choice and then Alice informs him of who has won, along with the value of her choice and the nonce she used to construct the hash. Again, Bob can verify the result but this time retains no real insight in determining what Alice chooses during the next round.

If we were to implement this game using Plutus, it would look something like this:

- Alice opens the game by committing her hash and 1 ADA
  - Bob doesn't answer and Alice retrieves her 1 ADA
  - Bob accepts the game by commiting Alice's hash, his choice, and 1 ADA
    - Alice realizes that she has won and reveals her choice. The game ends and she collects 2 ADA.
    - Alice realizes that she has lost and simply waits for a deadline to pass. Bob collects 2 ADA.
