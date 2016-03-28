giraffe.messages <- function(){
    msgs <- c("Hightopsisthecutestgiraffeintheworld",
              "Lions are stupid",
              "Leafy McLeafyface",
              "https://www.youtube.com/watch?v=vh3Rcl-2wDE")
    cat(msgs[sample(1:length(msgs), size = 1)], "\n")
}

`+` <- function(e1, e2){
    giraffe.messages()
    .Primitive("+")(e1, e2)
}

`-` <- function(e1, e2){
    giraffe.messages()
    .Primitive("-")(e1, e2)
}

`/` <- function(e1, e2){
    giraffe.messages()
    .Primitive("/")(e1, e2)
}

`*` <- function(e1, e2){
    giraffe.messages()
    .Primitive("*")(e1, e2)
}
