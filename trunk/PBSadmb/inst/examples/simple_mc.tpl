// Simple linear regression model with MCMC sampling
// Adapted from the example at http://otter-rsch.com/examples.htm
// Report designed for import by the PBSmodelling function "readList"

// Step 1. This command runs a small test that makes the binary file simpleMC.mcm:
// simpleMC -mcmc 1000 -mcsave 20

// Step 2. This command writes text data extracted from the binary file:
// simpleMC -mceval > simpleMC.mc.dat
// The output file "simpleMC.mc.dat" has 50 rows (1000/20) and 4 columns.

// In PBSadmb, these two steps can be implemented via the GUI or
// the function "runMC".

// The function "mceval_phase()" is true during the second calculation.


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

  // The MCMC iteration require at least one sdreport.
  // As a curiousity, consider the composite paramter a*b
  sdreport_number ab

PROCEDURE_SECTION
  ab = a*b;                                         // composite parameter
  ypred = a + b*x;                                  // linear prediction
  fval = norm2(ypred-y);                            // sum of squared residuals
  fval = nobs*log(sigma) + fval/(2.0*sigma*sigma);  // negative log likelihood

  if ( mceval_phase() ) {
    // Write a line with the 5 sampled values
    cout << a << " " << b << " " << sigma << " " << ab << " " << fval << endl; 
  }

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
  report << "$mcnames" << endl;
  report << "a b sigma ab fval" << endl;
  report << "$mcest" << endl;
  report << a << " " << b << " " << sigma << " " << ab << " " << fval << endl;
