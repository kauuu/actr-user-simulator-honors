The relevant files for the user model are: 
1) The model is present in `ACT-R/tutorial/value-user-model.lisp`. This includes the production rules and other functions for simulating the user behaviour.
2) The environment is present in `ACT-R/tutorial/python/value_experiment.py`. This includes the implementation of the environment. The model does not have be to initiated separelty. Running this python file will call the model as well.

To run, first import `value_experiment.py` and then call `value_experiment.experiment()` with no parameters. 
