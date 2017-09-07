color10 <-
function(n){
if(n<1 | n> 10){
stop("n must be 1 to 10")
}
acol <- c("#FFFFFF33", "#F9E2E248", "#F3C6C65E", "#EEAAAA74", "#E88D8D8A", 
"#E271719F","#DD5555B5", "#D73838CB","#D11C1CE1", "#CC0000F7")
return(rev(acol[10:(10-n+1)]))
}
