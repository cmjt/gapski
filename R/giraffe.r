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

giraffe.picture <- function(){
    giraffe.urls <- c("http://creationrevolution.com/wp-content/uploads/2013/01/9078giraffe.jpg",
                      "http://cdn.images.express.co.uk/img/dynamic/128/590x/Giraffes-in-the-wild-that-are-in-danger-543940.jpg",
                      "http://www.onekind.org/uploads/a-z/az_giraffe1.jpg",
                      "https://www.marwell.org.uk/images/main-images/giraffe-and-sky.jpg",
                      "http://giraffefacts.org/thecontent/images/2014/02/Giraffe%20eating%20leaves.jpg",
                      "http://i.dailymail.co.uk/i/pix/2009/07/21/article-0-05C980A5000005DC-696_634x445.jpg")
    url <- giraffe.urls[sample(1:length(giraffe.urls), size = 1)]
    system(paste("google-chrome", url))
}
