A=$<.read.split.map(&:to_i);B=A[2..].zip(A[1..], A).map(&:sum);p B[1..].zip(B).filter{|a,b|a>b}.length
