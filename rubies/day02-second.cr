x,_,y = ARGF.gets_to_end.strip.lines.map do |s|
	a,b=s.split
	{a,b.to_i}
end.reduce({0,0,0}) do |s,v|
	pos,aim,depth = s
	cmd,val = v

	case cmd
	when "forward"
		{pos+val, aim, depth + aim*val}
	when "down"
		{pos, aim+val, depth}
	when "up"
		{pos, aim-val, depth}
	else
		{pos, aim, depth}
	end
end

p x*y
