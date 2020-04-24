https://htdp.org/2018-01-06/Book/part_five.html

https://papers.nips.cc/paper/7445-neural-guided-constraint-logic-programming-for-program-synthesis.pdf
Very very very important: https://docs.racket-lang.org/math/dist_dist-objects.html

Code: https://jeapostrophe.github.io/home/static/toronto-2010ifl.pdf https://github.com/ShangtongZhang/reinforcement-learning-an-introduction


https://stackoverflow.com/questions/33428589/pyspark-and-pca-how-can-i-extract-the-eigenvectors-of-this-pca-how-can-i-calcu/33500704#33500704

 Fixed Clause Length (FCL)  and Constant Probability
https://www.aaai.org/Papers/AAAI/1998/AAAI98-036.pdf

https://github.com/ShangtongZhang/reinforcement-learning-an-introduction

https://scholarworks.iu.edu/dspace/bitstream/handle/2022/25183/Constraint_Microkanren_in_the_CLP_Scheme.pdf?sequence=1&isAllowed=y

http://www.diva-portal.org/smash/get/diva2:1020559/FULLTEXT02.pdf

https://mitpress.mit.edu/books/reasoned-schemer-second-edition

Free version: 
https://kupdf.net/download/the-reasoned-schemerpdf_5985ad99dc0d60486c300d18_pdf


https://github.com/jpt4/miniKanren-with-symbolic-constraints/blob/ab211226433c5dd1461df93f100a85968c628d93/README.md
Good for writing Quine-generating interpreters, etc. :)
 
http://io.livecode.ch/learn/webyrd/webmk
Also includes eigen, which represents universally quanitifed variables. Beware: this implementation does not support use of eigen with constraints other than ==.

https://lexi-lambda.github.io/blog/2018/09/13/custom-core-forms-in-racket-part-ii-generalizing-to-arbitrary-expressions-and-internal-definitions/

https://github.com/lexi-lambda/hackett/tree/master/hackett-demo/hackett/demohttps://github.com/lexi-lambda/hackett/tree/master/hackett-demo/hackett/demo

http://incompleteideas.net/book/RLbook2018.pdf

10.3. Average Reward: A New Problem Setting for Continuing Tasks 249  RLbook2018 to page 256

Page 266 to 268

288 to 300 

300 to 313

The Futility of Discounting in Continuing Problems Perhaps discounting can be saved by choosing an objective that sums discounted values over the distribution with which states occur under the policy: J(⇡) = X s µ⇡(s)v ⇡(s) (where v ⇡ is the discounted value function) = X s µ⇡(s) X a ⇡(a|s) X s0 X r p(s0 , r|s, a) [r + v ⇡(s0 )] (Bellman Eq.) = r(⇡) +X s µ⇡(s) X a ⇡(a|s) X s0 X r p(s0 , r|s, a)v ⇡(s0 ) (from (10.7)) = r(⇡) + X s0 v ⇡(s0 ) X s µ⇡(s) X a ⇡(a|s)p(s0 |s, a) (from (3.4)) = r(⇡) + X s0 v ⇡(s0 )µ⇡(s0 ) (from (10.8)) = r(⇡) + J(⇡) = r(⇡) + r(⇡) + 2J(⇡) = r(⇡) + r(⇡) + 2r(⇡) + 3r(⇡) + ··· = 1 1 r(⇡). The proposed discounted objective orders policies identically to the undiscounted (average reward) objective. The discount rate does not influence the ordering!


Di↵erential semi-gradient n-step Sarsa for estimating qˆ ⇡ q⇡ or q⇤ Input: a di↵erentiable function ˆq : S ⇥ A ⇥ Rd ! R, a policy ⇡ Initialize value-function weights w 2 Rd arbitrarily (e.g., w = 0) Initialize average-reward estimate R¯ 2 R arbitrarily (e.g., R¯ = 0) Algorithm parameters: step size ↵, > 0, a positive integer n All store and access operations (St, At, and Rt) can take their index mod n + 1 Initialize and store S0 and A0 Loop for each step, t = 0, 1, 2,... : Take action At Observe and store the next reward as Rt+1 and the next state as St+1 Select and store an action At+1 ⇠ ⇡(·|St+1), or "-greedy wrt ˆq(St+1, ·, w) ⌧ t n +1 (⌧ is the time whose estimate is being updated) If ⌧ 0: P⌧+n i=⌧+1(Ri R¯)+ˆq(S⌧+n, A⌧+n, w) qˆ(S⌧ , A⌧ , w) R¯ R¯ + w w + ↵rqˆ(S⌧ , A⌧ , w)

9 In the di↵erential semi-gradient n-step Sarsa algorithm, the step-size parameter on the average reward, , needs to be quite small so that R¯ becomes a good long-term estimate of the average reward

To develop intuitions, consider the case with three states S = {s1, s2, s3} and two parameters w = (w1, w2)>. We can then view all value functions/vectors as points in a three-dimensional space. The parameters provide an alternative coordinate system over a two-dimensional subspace. Any weight vector w = (w1, w2)> is a point in the two-dimensional subspace and thus also a complete value function vw that assigns values to all three states. With general function approximation the relationship between the full space and the subspace of representable functions could be complex, but in the case of linear value-function approximation the subspace is a simple plane, as suggested by Figure 11.3

The projection matrix For a linear function approximator, the projection operation is linear, which implies that it can be represented as an |S| ⇥ |S| matrix: ⇧ . = X X>DX1 X>D, (11.14) where, as in Section 9.4, D denotes the |S| ⇥ |S| diagonal matrix with the µ(s) on the diagonal, and X denotes the |S| ⇥ d matrix whose rows are the feature vectors x(s)>, one for each state s. If the inverse in (11.14) does not exist, then the pseudoinverse is substituted. Using these matrices, the squared norm of a vector can be written kvk 2 µ = v>Dv, (11.15) and the approximate linear value function can be written vw = Xw. (11.16) The true value function v⇡ is the only value function that solves (11.13) exactly. If an approximate value function vw were substituted for v⇡, the di↵erence between the right and left sides of the modified equation could be used as a measure of how far o↵ vw is from v⇡. We call this the Bellman error at state s: ¯w(s) . = 0 @X a ⇡(a|s) X s0,r p(s0 , r|s, a) [r + vw(s0 )] 1 A vw(s) (11.17) = E⇡ ⇥ Rt+1 + vw(St+1) vw(St) St = s, At ⇠ ⇡ ⇤ ,


TD(λ) Return algorithm

Input: the policy ⇡ to be evaluated Input: a di↵erentiable function ˆv : S+ ⇥ Rd ! R such that ˆv(terminal,·)=0 Algorithm parameters: step size ↵ > 0, trace decay rate 2 [0, 1] Initialize value-function weights w arbitrarily (e.g., w = 0) Loop for each episode: Initialize S z 0 (a d-dimensional vector) Loop for each step of episode: | Choose A ⇠ ⇡(·|S) | Take action A, observe R, S0 | z z + rvˆ(S,w) | R + vˆ(S0 ,w) vˆ(S,w) | w w + ↵z | S S0 until S0 is terminal




https://bitbucket.org/spacer/workspace/projects/PROJ

http://seahorn.github.io/

https://github.com/shaobo-he/sls

https://github.com/soarlab/OL1V3R

https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/logical-indexing-on-hb/
