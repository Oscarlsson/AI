%Format sucks! data matrix should be (documents times words) where words is 
%all possible words in all documents

function run_perceptron() 

load('../data.mat') ; 

n = length(wordcount) 

max_index = -1 ; 
max_words = 0 ; 

for i = 1 : n
        len = length(wordcount{i}.id); 
        if (len > max_words)
           max_words = len ; 
           max_index = i ;  
   end 
end

max_index ; 
max_words ; 
data = zeros(n,max_words) ; 

for i = 1 : n 
    fill = max_words - length(wordcount{i}.id); 
    wordcount{i}.cnt = [wordcount{i}.cnt zeros(1,fill)]; 
    wordcount{i}.id  = [wordcount{i}.id (ones(1,fill)*(-1))] ; 
    data(i,:) = wordcount{i}.cnt ;   
end

end 
