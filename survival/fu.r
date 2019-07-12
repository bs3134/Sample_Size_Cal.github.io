woc=
  function(l1,l2,totaltime,accrualtime,delta,alpha,power){
    z1_value = qnorm(alpha)
    z2_value = qnorm((1-power)/2)
    var1 = (l1^2)/(1+(1/exp(l1*totaltime)-exp(l1*(accrualtime-totaltime)))/(l1*accrualtime))
    var2 = (l2^2)/(1+(1/exp(l2*totaltime)-exp(l2*(accrualtime-totaltime)))/l2*accrualtime)
    size = ((z1_value+z2_value)^2)*(var1+var2)/((delta-abs(l1-l2))^2)
    return(list(sample_size = size))
  }
