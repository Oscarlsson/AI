%TODO: fixa plot för att visa hur många iterationer som behövs innan den konvergerar. 
% träna på allt och testa på allt ger runt 74%

function [percent] = run_perceptron() 

load('output_final.mat'); 
nDocuments = length(wordcount) 
max_id = 0; 

%find highest word id
for i = 1 : nDocuments
   idarray = wordcount{i}.id;
   max_elem = max(idarray);
   if (max_elem > max_id)
      max_id = max_elem ; 
   end 
end
max_id

%initalize data matrix for perceptron
data = zeros(n,max_id); 

%update data matrix with cnt, for each document
for i = 1 : nDocuments
    id = wordcount{i}.id; 
    cnt = wordcount{i}.cnt;
    for j = 1 : length(id)
        data(i,id(j)) = cnt(j);
    end
       
end

%training on everthing!
load('categories.mat');
pos = categories.pos; 
w = perceptron(data, pos);

%testing on everything
percent = sum(sign((data*w)') == pos)/length(pos);

end 
