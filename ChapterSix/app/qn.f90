program qn
    use iso_fortran_env, only: stdin => input_unit, stdout=>output_unit, stderr=>error_unit
    implicit none

    integer :: fileunit, stat
    character(len=9999):: filename, text, action, pos
    logical::exists

    !! handle error with stop
    if (command_argument_count()<1) stop 'Usage: qn <filename>'
    call get_command_argument(1,filename)

    inquire(file=trim(filename),exist=exists)

    if(exists) then
        write(stdout,"(a)") "File "//trim(filename)//" already exits"
        write(stdout,"(a)", advance='no') '[O]verwrite, [A]ppend, [Q]uit: '
        read(stdin,*) action
    end if

    pos = "rewind"

    if(exists) then
        if (any(action ==["O","o"])) then
            write(stdout,"(a)") "Overwriting file"
        else if (any(action ==["A","a"])) then
            write(stdout,"(a)") "Appending in file"
            pos  = "append"
        else if (any(action ==["Q","q"])) then
            stop 'Quit program'
        end if
    endif

    open(newunit=fileunit, file=trim(filename), position=trim(pos), action="write")

    do
        read(stdin,"(a)",iostat=stat,err=100) text
        write(fileunit,'(a)',iostat=stat,err=100) trim(text)
        flush(fileunit,iostat=stat,err=100)
    end do

    100 close(fileunit)
    if(stat>0) then
        write(stderr, "(a,i13)") "Error encountered, code=", stat
        stop
    endif

    

end program qn