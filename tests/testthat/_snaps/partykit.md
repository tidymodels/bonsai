# condition inference trees

    Code
      set_mode(set_engine(decision_tree(), "partykit"), "regression")
    Output
      Decision Tree Model Specification (regression)
      
      Computational engine: partykit 
      

---

    Code
      set_mode(set_engine(decision_tree(), "partykit", teststat = "maximum"),
      "classification")
    Output
      Decision Tree Model Specification (classification)
      
      Engine-Specific Arguments:
        teststat = maximum
      
      Computational engine: partykit 
      

# condition inference forests

    Code
      set_mode(set_engine(rand_forest(), "partykit"), "regression")
    Output
      Random Forest Model Specification (regression)
      
      Computational engine: partykit 
      

---

    Code
      set_mode(set_engine(rand_forest(), "partykit", teststat = "maximum"),
      "classification")
    Output
      Random Forest Model Specification (classification)
      
      Engine-Specific Arguments:
        teststat = maximum
      
      Computational engine: partykit 
      

