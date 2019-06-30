




server <-  function(input, output) {
    
    
    
    mydata <-  reactive ({ xyz <- input$char_vec
    
    words <- strsplit(xyz, " ")
    
    words <- unlist(words)
    
    words <- gsub("[[:punct:]]", "", words)
    
    words <- tolower(words)
    
    
    })
    output$submit <- renderUI({
        
        
        fluidRow(actionButton("submit1", label="submit"))
        
        
        
    })
    
   
    
    txt_2 <- eventReactive (input$submit1,  {  last_words <- tail(mydata(), 4)
    
    
    if (length(last_words) < 4) {returned_object <- c("Please enter at least 4 words")
    
    return(returned_object)}
    
    else { 
        
        
        filtered_five <-  sepfive[which(sepfive$word1==last_words[1] & sepfive$word2==last_words[2] & sepfive$word3==last_words[3]
                                        & sepfive$word4==last_words[4]), ] 
        
        
        if ( nrow(filtered_five) >= 3) { returned_object <- filtered_five$word5[1:3]
        return(returned_object)}
        
        ## fine primo statement
        
        else { filtered_four <-sepfour[which(sepfour$word1==last_words[2] & sepfour$word2==last_words[3] & sepfour$word3==last_words[4]), ]
        
        
        
        if (nrow(filtered_four) >=3) { sep_five2 <- sepfive[which(sepfive$word2==last_words[2] & sepfive$word3==last_words[3] & sepfive$word4==last_words[4]), ]
        
        summarized <- sep_five2 %>% group_by(word5) %>% summarise(no_rows=length(word5))
        
        summarized <- as.data.frame(summarized)
        
        summarized <- summarized[order(-summarized$no_rows),]
        
        four_freq <- (summarized$no_rows-0.75)/nrow(sep_five2)
        zeros_four <- rep(0, nrow(summarized))
        max_four <- pmax(four_freq, zeros_four)
        lambda <- (0.75/sum(filtered_four$frequency))*nrow(filtered_four)
        sep_four2 <- sepfour[sepfour$word4 %in% filtered_four$word4, ]   
        pcon <- nrow(sep_four2)/nrow(sepfour)
        adj <- lambda*pcon
        
        kneser <- max_four+adj
        
        summarized$kneser <- kneser
        
        summarized <- summarized[order(-summarized$kneser), ]
        
        returned_object <- summarized$word5[1:3]
        
        return(returned_object) 
        }
        
     ## fine secondo statement    
        else { filtered_tri <-septri[which(septri$word1==last_words[3] & septri$word2==last_words[4]), ]
        
        
        if (nrow(filtered_tri) >=3) { sep_five3 <- sepfive[which( sepfive$word3==last_words[3] & sepfive$word4==last_words[4]), ]
        
        summarized2 <- sep_five3 %>% group_by(word5) %>% summarise(no_rows=length(word5))
        
        summarized2 <- as.data.frame(summarized2)
        
        summarized2 <- summarized2[order(-summarized2$no_rows),]
        
        tri_freq <- (summarized2$no_rows-1.25)/nrow(sep_five3)
        zeros_tri <- rep(0, nrow(summarized2))
        max_tri <- pmax(tri_freq, zeros_tri)
        lambda2 <- (1.25/sum(filtered_tri$frequency))*nrow(filtered_tri)
        
        sep_tri2 <- septri[septri$word3 %in% filtered_tri$word3, ]   
        pcon2 <- nrow(sep_tri2)/nrow(septri)
        adj2 <- lambda2*pcon2  
        kneser2 <- max_tri+adj2
        
        
        summarized2$kneser2 <- kneser2
        
        summarized2 <- summarized2[order(-summarized2$kneser2),]
        
        returned_object <- summarized2$word5[1:3]
        
        return(returned_object)
        } 
        
        ## fine terzo statement 
        
        else {filtered_bi <-sepbi[which(sepbi$word1==last_words[4]) , ]
        
        if (nrow(filtered_bi) <=2 )  { sample_1 <- sample(sepone$feature, 10)
            
            random <- sepone[which(sepone$feature %in%sample_1), ]
            
            random <- random[order(-sepone$frequency)]
        return(random$feature[1:3]) }
        
        
        else  { sep_five4 <- sepfive[which( sepfive$word4==last_words[4]), ]
        
        summarized3 <- sep_five4 %>% group_by(word5) %>% summarise(no_rows=length(word5))
        
        summarized3 <- as.data.frame(summarized3)
        
        summarized3 <- summarized3[order(-summarized3$no_rows),] 
        bi_freq <- (summarized3$no_rows-1.25)/nrow(sep_five4)
        zeros_bi <- rep(0, nrow(summarized3))
        max_bi <- pmax(bi_freq, zeros_bi)
        lambda3 <- (1.25/sum(filtered_bi$frequency))*nrow(filtered_bi)
        
        sep_bi2 <- sepbi[sepbi$word2 %in% filtered_bi$word2, ]   
        pcon3 <- nrow(sep_bi2)/nrow(sepbi)
        adj3 <- lambda3*pcon3  
        kneser3 <- max_bi+adj3
        
        
        summarized3$kneser3 <- kneser3
        summarized3 <- summarized3[order(-summarized3$kneser3), ]
        
        returned_object <- summarized3$word5[1:3]
        return(returned_object)
        
        
        }
        
        ## fine quarto statement 
        
        
        }      
        
        }
        
      
        }    
      
    
}
    
    })
    
    
    output$txt_pred <- renderPrint(txt_2())
    
}
    
    
    
    