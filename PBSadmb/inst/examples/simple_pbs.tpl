// Simple linear regression model
// Adapted from the example at http://otter-rsch.com/examples.htm
// Report designed for import by the PBSmodelling function "readList"
// Try the <Import> button on the PBSadmb GUI with "rep" selected.

DATA_SECTION
  init_int nobs
  init_vector y(1,nobs)
  init_vector x(1,nobs)

PARAMETER_SECTION
  init_number a
  init_number b
  init_bounded_number sigma(0.0001,10000.0)  // sigma > 0
  vector ypred(1,nobs)
  objective_function_value fval

PROCEDURE_SECTION
  ypred = a + b*x;                                  // linear prediction
  fval = norm2(ypred-y);                            // sum of squared residuals
  fval = nobs*log(sigma) + fval/(2.0*sigma*sigma);  // negative log likelihood

REPORT_SECTION
  report << "$a"      << endl;
  report << a         << endl;
  report << "$b"      << endl;
  report << b         << endl;
  report << "$sigma"  << endl;
  report << sigma     << endl;
  report << "$fval"   << endl;
  report << fval      << endl;
  report << "$x"      << endl;
  report << x         << endl;
  report << "$y"      << endl;
  report << y         << endl;
  report << "$ypred"  << endl;
  report << ypred     << endl;
