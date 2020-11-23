// STL
#include <iostream>
#include <algorithm> 
#include <cmath>
// SCYTHE
#include "matrix.h" 
#include "distributions.h"
#include "ide.h" 
#include "la.h"
#include "mersenne.h"
#include "rng.h"
#include "stat.h" 
#include "smath.h" 
// R interface
#include <R.h>           
#include <R_ext/Utils.h> 
#include <Rdefines.h>
#include <Rinternals.h>
// 
#include "extra.h"
#include "cumres.h"

#ifdef _OPENMP
#include <omp.h>
#endif

extern const double SDtol = 1e-9;
//*************************************************


using namespace scythe;
using namespace std;

double KolmogorovSmirnov(const scythe::Matrix<double> &W) {
  return(scythe::max(scythe::fabs(W)));

}

double CramerVonMises (const scythe::Matrix<double> &W, const scythe::Matrix<double> &It, unsigned var) {
  unsigned n = It.rows();
  double p2 = It.cols();
  unsigned p=sqrt(p2);
  
  Matrix<double> Itd(1,p2);
  Matrix<double> Itd1(1,p2);
  
  Matrix<double>CvM(n,1);
  
  for (unsigned i=0; i<(n-1); i++) {
    Itd = It(i,_); Itd.resize(p,p);
    Itd1 = It(i+1,_); Itd1.resize(p,p);
    CvM[i] = W[i]*W[i]*((Itd1(var,var)-Itd(var,var)));
    
  } 
  
  return(scythe::sum(CvM)); 
}

double AndersonDarling (const scythe::Matrix<double> &W, const scythe::Matrix<double> &It, unsigned var) {
  unsigned n = It.rows();
  double p2 = It.cols();
  unsigned p=sqrt(p2);
  
  Matrix<double> Itdi = It(n-1,_); Itdi.resize(p,p);
  Matrix<double> Itd(1,p2);
  Matrix<double> Itd1(1,p2);
  
  unsigned Nprep=0;
  for (unsigned i=0; i<(n-1); i++) {
    Itd = It(i,_); Itd.resize(p,p);
    if ((Itd(var,var)*(1-Itd(var,var)/Itdi(var,var))!=0)){
    Nprep=Nprep+1;
    }
  }
  
  Matrix<double>ASprep(Nprep,1);
  
  unsigned N=0;
  for (unsigned i=0; i<(n-1); i++) {
    Itd = It(i,_); Itd.resize(p,p);
    Itd1 = It(i+1,_); Itd1.resize(p,p);
    if ((Itd(var,var)/Itdi(var,var)*(1-Itd(var,var)/Itdi(var,var))!=0)){
    ASprep[N] = W[i]*W[i]*((Itd1(var,var)-Itd(var,var))/(Itd(var,var)*(1-Itd(var,var)/Itdi(var,var))));
    N=N+1;
    }
  }
  
  return(scythe::sum(ASprep)); 
}

scythe::Matrix<double> ProdMat(const scythe::Matrix<double> &A,
                               const scythe::Matrix<double> &B) {
                                 
Matrix<double> C(A.rows(), B.cols());
unsigned i,j,k;

#ifdef _OPENMP
#pragma omp parallel shared(A,B,C) private(i,j,k)
{
#pragma omp for schedule(static)
#endif

  for (i=0; i<A.rows(); i++)
  {
    for (j = 0; j < B.cols(); j++)
    {
     for (k = 0; k < B.rows(); k++)
        {
         C(i, j) +=A(i,k)*B(k,j);
        }
    }
  }
#ifdef _OPENMP
}
#endif

return(C);
}

scythe::Matrix<> SumMat(const scythe::Matrix<> &M) {
  unsigned n = M.rows();
  unsigned m = M.cols();
  if (M.rows()==1)
    n = M.cols();    
  Matrix<> res(1, M.cols());  
for (unsigned i=0; i<n; i++) {
    for (unsigned j=0; j<m; j++) {
    res[j] +=M(i,j);
  }
}
  return(res);
}

scythe::Matrix<> cumsum(const scythe::Matrix<> &M) {
  unsigned n = M.rows(); 
  unsigned m = M.cols();
  if (M.rows()==1)
    n = M.cols();    
  Matrix<> res(M.rows(), M.cols());  
  res(0,scythe::_) = M(0,scythe::_);

  for (unsigned i=1; i<n; i++) {
    for (unsigned j=0; j<m; j++) {
    res(i,j) = res(i-1,j)+M(i,j);
  }
}
  return(res);
}





