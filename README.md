# Prolexa
Computational Logic - Mauro Comi, Subin Park


## Negation
In this exercise, we want to handle the following reasoning pattern: 
> Every teacher is happy. Pixie is not happy. Therefore, Pixie is not a teacher.

The default prolexa version does not allow facts with negated predicates, such as `pixie is not happy`. So, we define a negative verb phrase as follows:
```
neg_verb_phrase(s,M) --> [is,not],property(s,M).
```
Prolexa is now able to receive facts composed of [is] + [not] + predicate. The statement `pixie is not happy` can be expressed in prolog as `not(happy(pixie)):-true`. Therefore, we create this rule by adding the following line to the grammar:
```
sentence1([(not(L):-true)]) --> proper_noun(N,X),neg_verb_phrase(N,X=>L).
```
The query is now handled correctly by prolog and the corresponding rule is added to the Rulebase:
```
prolexa> "pixie is not happy".
*** utterance(pixie is not happy)
*** rule([(not(happy(pixie)):-true)])
*** answer(I will remember that pixie is not happy)
I will remember that pixie is not happy
```

However, if we ask the query "is pixie happy", we don't receive a definitive answer:
```
prolexa> "is pixie happy".
*** utterance(is pixie happy)
*** query(happy(pixie))
*** answer(Sorry, I don't think this is the case)
Sorry, I don't think this is the case
```
"Sorry I don't think this is the case" is the string that Prolexa outputs if the statement is false or if the statement is not well defined. This is expected, because our query searches for a rule in the form `happy(pixie)):-B`, which is not stored in our Rulebase. We want to fix this, as we want to have a clear answer on the query "is pixie happy". Thus, we extend `prove_question/3` by adding `prove_rb(not(Query),Rulebase)`:

```
prove_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),     % create a list of all the rules and store them in RuleBase
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
 	; prove_rb(not(Query),Rulebase) ->
		transform(not(Query),Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).
```

Now Prolexa can search for a rule obtained by negating the query:
```
prolexa> "is pixie happy".
*** utterance(is pixie happy)
*** query(happy(pixie))
*** answer(pixie is not happy)
pixie is not happy
```
The last challenge that we need to tackle consists in inferring that pixie is not a teacher, since pixie is not happy and every teacher is happy. This cannot be handled by the current version of Prolexa, because the query `teacher(pixie)` looks for a rule in the form `teacher(pixie):-A`, which does not exist. To solve this, we add a `prove_rb/4` rule as follows:
```
prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).

prove_rb(not B,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
	prove_rb(not A,Rulebase,[p(not B,Rule)|P0],P)
```
Given the query "is pixie a teacher", Prolexa converts it into `query(teacher(pixie))`. The rule `prove_rb/4` implemented above first looks for a clause that satisfies the rule `teacher(X):-B`. Since this does not exist, Prolexa runs the method `prove_rb(not(Query), Rulebase)` inside `prove_question/3`.  Therefore, `not(Query)=not(teacher(pixie)`. Then, Prolexa runs the method `prove_rb(not B,Rulebase,P0,P)` shown above and instantiates `B=teacher(pixie)`. The goal is to find a clause in the form `A:-teacher(pixie)`, which succeeds and returns `A=happy(X)`. Finally, Prolexa runs the line `prove_rb(not A,Rulebase,[p(not B,Rule)|P0],P)` and succeeds, returning the rule `happy(X):-true`. Therefore, Prolexa is able to answer the query "is pixie a teacher". 
```
prolexa> "is pixie a teacher".
*** utterance(is pixie a teacher)
*** query(teacher(pixie))
*** answer(pixie is not teacher)
pixie is not teacher
```
The same rules explained above are also used by Prolexa to explain why pixie is not a teacher:
```
prolexa> "explain why pixie is not a teacher".
*** utterance(explain why pixie is not a teacher)
*** goal(explain_question(not(teacher(pixie)),_25412,_25138))
*** answer(pixie is not happy; every teacher is happy; therefore pixie is not teacher)
pixie is not happy; every teacher is happy; therefore pixie is not teacher
```
The method we have just implemented is the *modus tollens* deductive argument: `If P then Q. Not Q. Therefore not P.` Applying this argument to our specific case, we obtain: `If teacher(X), then happy(X). Not happy(X). Therefore, not teacher(X)`.

## Existential quantification
Prolexa can handle another type of reasoning called existential quantification rather limitedly than universal quantification. We are trying to handle the following logic:
> Some humans are innovative. Innovators are creative. Therefore, some humans are creative.

The default prolexa can handle determiners such as "all" and "every", but to process "some", we need to add either of grammar rules:
```
determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2:-true)]) --> [some].
```
```
determiner(p, sk=>H1, sk=>H2, [(H1,H2):-true]) --> [some].
```
Then, Prolexa understands that 'some' humans are innovative, not 'all' humans. First of all, rules about innovative humans and their creativity are added as:
```
prolexa> "some humans are innovative".
*** utterance(some humans are innovative)
*** rule([(human(sk):-true),(innovative(sk):-true)])
*** answer(I will remember that some humans are innovative)
I will remember that some humans are innovative
prolexa> "innovators are creative".
*** utterance(innovators are creative)
*** rule([(creative(_49934):-innovative(_49934))])
*** answer(I will remember that innovators are creative)
I will remember that innovators are creative
```
Therefore, Prolexa knows that some humans are innovative and all innovative humans are creative. Then, we handle two humans one of which is innovative and the other is not. 
```
prolexa> "donald is a human".
*** utterance(donald is a human)
*** rule([(human(donald):-true)])
*** answer(I will remember that donald is a human)
I will remember that donald is a human
prolexa> "donald is innovative".
*** utterance(donald is innovative)
*** rule([(innovative(donald):-true)])
*** answer(I will remember that donald is innovative)
I will remember that donald is innovative
prolexa> "is donald creative".
*** utterance(is donald creative)
*** query(creative(donald))
*** answer(donald is creative)
donald is creative
prolexa> "explain why donald is creative".
*** utterance(explain why donald is creative)
*** goal(explain_question(creative(donald),_55216,_55204))
*** answer(donald is innovative; every innovator is creative; therefore donald is creative)
donald is innovative; every innovator is creative; therefore donald is creative
```
Prolexa clearly understands why donald is creative. On the other hand, Prolexa cannot guarantee that a human who is not known to be innovative is creative.
```
prolexa> "pete is a human".
*** utterance(pete is a human)
*** rule([(human(pete):-true)])
*** answer(I will remember that pete is a human)
I will remember that pete is a human
prolexa> "is pete innovative".
*** utterance(is pete innovative)
*** query(innovative(pete))
*** answer(Sorry, I don't think this is the case)
Sorry, I don't think this is the case
```
However, even though a new rule is added, so we know that pete is not innovative, Prolexa still cannot conclude that pete is not creative. 
```
prolexa> "pete is not innovative".
*** utterance(pete is not innovative)
*** rule([(not(innovative(pete)):-true)])
*** answer(I will remember that pete is not innovative)
I will remember that pete is not innovative
prolexa> "is pete creative".
*** utterance(is pete creative)
*** query(creative(pete))
*** answer(Sorry, I don't think this is the case)
Sorry, I don't think this is the case
```
Finally, Prolexa cannot confirm which humans are creative only given that some humans are creative despite added rules.
