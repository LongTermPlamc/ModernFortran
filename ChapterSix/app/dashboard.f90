program dashboard
    use iso_fortran_env, only: stdin=>input_unit,stdout=>output_unit,stderr=>error_unit
    implicit none

    real :: lat=59.329444, lon=18.068611, alt=11678.3
    integer::eng(4) = [96,96,95,97]
    logical::airborne = .true.

    character(len=:), allocatable ::dashfmt,dashfmt2

    dashfmt ="(2f13.5,2X,f13.1,2X,4I3.2,2X,L2)"
    dashfmt2 = '(2(f9.5, 2X), f7.1, 2X, 4(I3.3,2X), L)'
    
    write(unit=stdout,fmt=dashfmt) lat,lon,alt, eng, airborne
    write(unit=stdout,fmt=dashfmt2) lat,lon,alt, eng, airborne

end program dashboard