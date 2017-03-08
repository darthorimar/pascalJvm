program;
    var i, j: integer;
begin
    for i := 1 to 10 do
    begin
        for j := 1 to 10 do
        begin
            write(i * j, ' ');
            if i * j < 10 then
                write(' ');
        end;
        writeln('');
    end;
end.