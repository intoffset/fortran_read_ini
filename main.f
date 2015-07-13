      program main
        use read_utility
        implicit none
        integer :: ival
        integer,dimension(3) :: ival3
        real*8 :: rval
        real*8,dimension(3) :: rval3
        character(50) :: sval
        open(1,file='test.ini',status='old')
        call read_param(1,'ival',ival)
        write(*,*)'ival is ',ival
        call read_param(1,'ival3',ival3)
        write(*,*)'ival3 is ',ival3
        call read_param(1,'rval',rval)
        write(*,*)'rval is ',rval
        call read_param(1,'rval3',rval3)
        write(*,*)'rval3 is ',rval3
        call read_param(1,'sval',sval)
        write(*,*)'sval is ',sval
        close(1)
        stop
      end program main
