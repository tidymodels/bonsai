# boost_tree with lightgbm

    Code
      boost_tree() %>% set_engine("lightgbm") %>% set_mode("regression")
    Output
      Boosted Tree Model Specification (regression)
      
      Computational engine: lightgbm 
      

---

    Code
      boost_tree() %>% set_engine("lightgbm", nrounds = 100) %>% set_mode(
        "classification")
    Output
      Boosted Tree Model Specification (classification)
      
      Engine-Specific Arguments:
        nrounds = 100
      
      Computational engine: lightgbm 
      

