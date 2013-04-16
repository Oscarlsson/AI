%Format sucks! data matrix should be (documents times words) where words is 
%all possible words in all documents

function run_perceptron() 

load('../data.mat'); 

n = length(wordcount) 
 
max_id = 0; 

for i = 1 : n
   len = wordcount{i}.id;
   max_elem = max(len);
   if (max_elem > max_id)
      max_id = max_elem ; 
   end 
end

max_id

data = zeros(n,max_id); 
for i = 1 : n
    id = wordcount{i}.id; 
    cnt = wordcount{i}.cnt;
    for j = 1 : length(id)
        data(i,id(j)) = cnt(j);
    end
       
end



end 
