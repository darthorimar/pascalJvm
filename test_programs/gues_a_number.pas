program;
    var guess: integer;
    const number = 64;
begin
    writeln('Guess the number form 1 to 100');
    while guess <> number do
    begin
        write('Guess the  number: ');
        read(guess);
        if guess > number then
            writeln('Too high');
        else if guess < number then
            writeln('Too low');
    end;
    writeln('Congrats!');
end.