library(rwebppl)


posterior <- webppl(program_file = "./R/test.wppl",
                    data_var = "data",
                    random_seed = 1234)