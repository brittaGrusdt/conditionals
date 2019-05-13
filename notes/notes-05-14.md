
## State generation

Sample 1000 tables for *each* network, i.e.
1. A independent C :  $P(A), P(C) \sim$  beta(1,1)

Add some noise to the generated table, because the probability that an independent network has generated exactly that table is 0.

```javascript
function(a, c){
  var epsilon = uniform({a:0, b:0.05})
  var table = map(function(t){t + epsilon},
                  [a*c, a * (1-c), (1-a) * c, (1-a) * (1-c)])
  var c = sum(table)

  return {"AC": table[0] / c, "A-C": table[1] / c,
          "-AC": table[2] / c, "-A-C": table[3] / c}
  })
```

2. Dependent nets:
    - $P(v_{\textrm{child}} \mid v_{\textrm{parent}}) \sim$ beta(**10,1**)
    - $P(v_{\textrm{child}} \mid \neg v_{\textrm{parent}}) \sim$ beta(**1,10**)
    - $P(v_{\textrm{parent}})\sim$ beta(1,1)


## State Prior

Combine each causal network with each table via the likelihood function.
The prior probability of the respective <causal net, table> pair is specified by the likelihood function:

```javascript
var as_if_zero = 0.000001
var log_likelihood = function(state){
  var p = cn_to_prob(state)
  return
  state.cn == "A || C" ? (p == 0 ? -Math.log(as_if_zero)
                                 : -Math.log(p))
                       : Math.exp(Beta({a:10, b:1}).score(p))
}
```

Problem was with independent networks: we'd like a continuous function, and not only two values; one for the case that the table counts as
being independent and one for the case that it does not meet the independence condition.

Independence condition I implemented:

* take the absolute difference of $P(C) * P(A)$ and $P(A,C)$
* the smaller that value the more likely it is that the table was generated from the independent network
* as score for factor in webppl take  $-Math.log(p)$
