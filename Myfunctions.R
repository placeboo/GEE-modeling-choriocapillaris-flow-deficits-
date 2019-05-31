
ci_measure = function(age, dist, coefs, sd){
        
        predict_vec= c(1, age, dist)
        
        mean = sum(predict_vec * coefs)
        
        sd = sqrt(t(predict_vec) %*% var_mat %*% predict_vec)
        
        return(c(mean - 1.96 * sd, mean + 1.96 * sd))
}
