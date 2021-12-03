A=$<.read.split.map(&:to_i);p A[1..].zip(A).filter{|a,b| a>b}.length
