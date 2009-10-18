// Simple linear regression model
// Adapted from the example at http://otter-rsch.com/examples.htm

DATA_SECTION
  init_int nobs
  init_vector y(1,nobs)
  init_vector x(1,nobs)

PARAMETER_SECTION
  init_number a
  init_number b
  init_bounded_number sigma(0.0001,10000.0)  // sigma > 0
  vector ypred(1,nobs)
  objective_function_value f

PROCEDURE_SECTION
  ypred = a + b*x;                             // linear prediction
  f = norm2(ypred-y);                          // sum of squared residuals
  f = nobs*log(sigma) + f/(2.0*sigma*sigma);   // negative log likelihood

REPORT_SECTION
  report << "Final parameters:"    << endl << endl;
  report << "a          " << a     << endl;
  report << "b          " << b     << endl;
  report << "sigma      " << sigma << endl << endl;
  report << "Function:  " << f     << endl;
