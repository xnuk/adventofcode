A=$<.read.lines.map do |s|
	a,b=s.split
	[a,b.to_i]
end.group_by{|a,b|a}.map{|k,v|[k,v.map{|_,v|v}.sum]}.to_h
p A["forward"] * (A["down"] - A["up"])
