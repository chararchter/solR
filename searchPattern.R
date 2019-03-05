searchPattern = function(name, parameter){
    for (char in 1:length(name)){
        for (i in 1:length(parameter)){
            if (grepl(parameter[i], name[char])){
                return(parameter[i])
            }
        }
    }
}