(define function ())
;; output: true if F is true, false otherwise

;;QBF is quantified boolean formulae
; Q1x1...Qnxn*E(x1,...,xn)
;;Definition 1:  Given a k QBF of the form (2), we define the sets:
; Sigma and II as follows:
; Sigma = Xk U Xk-2 U ""
; II : Xk- 1 U Xk- 3 U "’" 
;; sigma is the union of all the sets of existentially quantified variables, while II collects all the universally quantified variables.

;;Example 1: Let F be the QBF
;; v is upside down A
; vw3zvx Y [(xl v ^ (yl)^
; v v ^
; v v ^ ( y2
; Then II = W U X -~ (Wl,W2,Xl,X2)
; , while Sigma. =
; Z U Y = (zl,yl,y2}.
;; using the sets sigma and II, we can partition the matrix E(X1, ..., Xk) into three clauses
; H(sigma), containing the clauses in which only variables
; of sigma occur;
; 2. G(H), containing the clauses in which only variables
; of H occur;
; 3. L(E, II), containing the remaining clauses.

; The truth
; assignment li = false for i = 1 .... ,m makes G
; false. Since all variables corresponding to the literals
; li,... ,lm are universally quantified, F is false. 

; The simple check
; quired in Lemma 1can be accomplished in time linear
; in the size of the matrix. As a consequence, in all the
; interesting cases we always have that G(H) = 0. In the
; following we assume that this is the case

; A QBF F of the form (4) is false if H(E) is unsatisfiable.

;  QBF F of the form (4)
; is true if H(E) A L’(E) is satisfiable, where L’(E)
; obtained from L(E, H) by deleting all variables in II.


; QBF VXSY[(xl v Yl)
; (-~ml V -~Yl)] is true while L’(E) -= Yl A -~Yl is unsatisfiable. Notice that verifying the condition of the above
; lemma requires to perform a satisfiability test. Nevertheless, our experimental analysis has shown that the
; presence of this test makes the algorithm Evaluate more
; efficient.

;;pretty much L’(E) is satisfiable, where L’(E) obtained from L(E, H) by deleting all variables in II. And then just a simple test of (-~ml V -~Yl)]

;  Given a QBF
; F of the form (2) and a monotone literal l E if,
; then F is true if and only if P = Q1Xi."BXk
; E’(X1,...,Xk) is true, where E’(X1,...,X~) is
; tained from E(X1,..., Xk ) by replacing I with false.

;;pretty much, if X1 is false, then F is true

;;6th clause doesn't really matter

; The main procedure Evaluate takes as input a QBF
; F and returns its truth value. As a matter of fact,
; it performs two simple actions: first of all, all tautological clauses in F (if any) are removed. Notice that
; the elimination of tautological clauses can be performed
; once and for all: in fact, none of the successive manipulations that the algorithm makes on the input formula
; can create a tautological clause. Then, Evaluate invokes
; either E_Evaluate or H_Evaluate according to whether F
; is a E-formula or a H-formula.
; Figure 2 shows the procedure H_Evaluate. The procedure takes in input a H-formula and returns its truth
; value; it is at the same time an iterative and recursive
; procedure, that works as follows.
; Base of recursion. First of all, H_Evaluate checks
; whether the input formula F is formed by all existentially quantified variables; in this case, F is indeed a
; 1QBF and then for evaluating it is sufficient to invoke any procedure for SAT
; formula F is true, and then H_Evaluate can stop and return true 

; all clauses are different, non-tautologous, and contain
; exactly h literals, not all of them universally quantified. If V is the set of all propositional variables in
; the formula (that is V = X1 (.J ... [J Xk)
; ,
; then a clause
; is produced by randomly choosing h different variables
; in V and negating each one with probability 0.5. The
; CP model has the same parameters of the FCL model,
; except for h, which represents in this case the average
; number of literals per clause in the formula. The empty
; clause and unit clauses are disallowed.