// von Bertalanffy growth curve with MCMC results
// The four parameters have "diffuse" (uniform) priors.
// See the PBSadmb Users Guide, Section 2.

// Run  'vonb -mcmc 1000 -mcsave 20' (small test that makes a binary file vonb.mcm)
// Then 'vonb -mceval > vonb.mc.dat' (writes text data from the binary file)
// In this case, vonb.mc.dat has 50 rows (1000/20) and 6 columns.

// Note the functions "mceval_phase()" and "mcOutput()".

DATA_SECTION
  init_int nobs                  // number of observations
  init_matrix data(1,nobs,1,2)   // age, length
  vector age(1,nobs)
  vector y(1,nobs)

PARAMETER_SECTION
  init_number Linf
  init_number K
  init_number t0
  init_number sigma
  vector ypred(1,nobs)           // predicted length 
  objective_function_value fval
  // The MCMC iteration requires at least one sdreport_number.
  sdreport_number LK             // Linf*K, a composite parameter
  likeprof_number VonBLinf       // Linf, renamed for likelihood profiling

PRELIMINARY_CALCS_SECTION
  age = column(data,1);
  y = column(data,2);

PROCEDURE_SECTION
  ypred = Linf*(1-exp(-K*(age-t0)));
  LK = Linf*K; VonBLinf = Linf;
  fval = norm2(ypred - y);                          // sum of squared residuals
  fval = nobs*log(sigma) + fval/(2.0*sigma*sigma);  // negative log likelihood

  if (mceval_phase()) mcOutput();

FUNCTION mcOutput
// Output parameters sampled; printed to 'cout' during the '-eval' phase.
// Pipe to a file, such as vbmc.dat. Each line has 6 values.
  cout << Linf << " " << K << " " << t0 << " " << sigma << " ";
  cout << LK << " " << fval << endl;

REPORT_SECTION
  report << "$Linf"   << endl;
  report << Linf      << endl;
  report << "$K"      << endl;
  report << K         << endl;
  report << "$t0"     << endl;
  report << t0        << endl;
  report << "$sigma"  << endl;
  report << sigma     << endl;
  report << "$fval"   << endl;
  report << fval      << endl;
  report << "$age"    << endl;
  report << age       << endl;
  report << "$y"      << endl;
  report << y         << endl;
  report << "$ypred"  << endl;
  report << ypred     << endl;
  report << "$mcnames" << endl;
  report << "Linf K t0 sigma LK fval" << endl;
  report << "$mcest" << endl;
  report << Linf << " " << K << " " << t0 << " " << sigma << " " << LK << " " << fval << endl;
