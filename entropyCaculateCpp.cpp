#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double entropyCaculateCpp(CharacterVector x) {
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

// // [[Rcpp::export]]
// NumericVector leftCpp(CharacterVector seg_words, std::vector<std::string> words) {
//   NumericVector result(words.size());
//   for (int i = 0; i < words.size(); i++) {
//     CharacterVector left, right;
//     std::string temp = as<std::string>words[i];
//     CharacterVector start = temp.substr(0, 1); 
//     Rcout << start[0] << std::endl;
//     CharacterVector end = temp.substr(temp.length(), 1);
//     Rcout << end[0] << std::endl;
// 
//     for (int j = 0; j < seg_words.size(); j++) {
//       if (start[0] == seg_words[j]) {
//         if (j >= 1) {
//           left.push_back(seg_words[j - 1]);
//         }
//       }
//       
//       if (end[0] == seg_words[j]) {
//         if (j <= (seg_words.size() - 1)) {
//           right.push_back(seg_words[j + 1]);
//         }
//       }
//     }
//     
//     double left_entropy, right_entropy;
//     left_entropy = entropyCaculateCpp(left);
//     right_entropy = entropyCaculateCpp(right);
//     result[i] = std::min(left_entropy, right_entropy);
//     
//   }
//   return result;
// }
