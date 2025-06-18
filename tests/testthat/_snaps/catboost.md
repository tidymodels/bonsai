# boost_tree with catboost

    Code
      set_mode(set_engine(boost_tree(), "catboost"), "regression")
    Output
      Boosted Tree Model Specification (regression)
      
      Computational engine: catboost 
      

---

    Code
      set_mode(set_engine(boost_tree(), "catboost", iterations = 100),
      "classification")
    Output
      Boosted Tree Model Specification (classification)
      
      Engine-Specific Arguments:
        iterations = 100
      
      Computational engine: catboost 
      

