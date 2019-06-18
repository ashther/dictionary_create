#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double entropyCaculate(CharacterVector x) {
  CharacterVector uniq = unique(x);
  std::map<String, int> temp;
  for (int i = 0; i < x.size(); i++) {
    temp[x[i]]++;
  }
  NumericVector result(uniq.size());
  for (int i = 0; i < uniq.size(); i++) {
    result[i] = temp[uniq[i]];
  }
  
  result = result / x.size();
  result = ifelse(result == 0, 0.0, result * log(result));
  return -1.0 * sum(result);
}

// [[Rcpp::export]]
List entropyGet(CharacterVector x_uniq, CharacterVector y) {
  NumericVector pre(x_uniq.size());
  NumericVector suff(x_uniq.size());
  
  for (int i = 0; i < x_uniq.size(); i++) {
    CharacterVector pre_temp;
    CharacterVector suff_temp;
    for (int j = 0; j < y.size(); j++) {
      if (y[j] == x_uniq[i]) {
        if (j >= 1) {
          pre_temp.push_back(y[j - 1]);
        }
        if (j < (y.size() - 1)) {
          suff_temp.push_back(y[j + 1]);
        }
      }
    }
    pre[i] = entropyCaculate(pre_temp);
    suff[i] = entropyCaculate(suff_temp);
  }
  
  return List::create(
    Named("w") = x_uniq, 
    Named("pre") = pre, 
    Named("suff") = suff
  );
}


/*** R
entropyCaculate(sample(letters, 100, TRUE))
*/
