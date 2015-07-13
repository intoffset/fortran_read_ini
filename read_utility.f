      module read_utility
        implicit none
        integer,parameter :: ru_max_line_length=100
        character(*),parameter :: ru_read_format='(A100)'
        interface read_param
          module procedure read_integer,read_integers,
     *                     read_real8,read_real8s,read_string
        end interface
      contains
        ! read integer value with key
        subroutine read_integer(nunit,key,val,nece,defval)
          integer,intent(in)          :: nunit  ! unit number
          character(*),intent(in)     :: key    ! key
          integer,intent(out)         :: val    ! return value
          logical,intent(in),optional :: nece   ! necessity
          integer,intent(in),optional :: defval ! default value
          logical :: nece_=.true. ! default of nece
          integer :: defval_=0    ! default of defval
          character(ru_max_line_length) :: line
          character(ru_max_line_length) :: readkey
          character(ru_max_line_length) :: readval
          integer :: st
          integer :: err
          rewind(nunit)
          read(nunit,ru_read_format,iostat=st)line
          do while(st.eq.0)
            call trim_comment(line)
            line=adjustl(line)
            if (len_trim(line).eq.0) then
              read(nunit,ru_read_format,iostat=st)line
              cycle
            endif
            err=split_str(line,readkey,readval,"=")
            if((readkey.eq.key).and.(err.eq.0))then
              read(readval,*,iostat=st)val
              if(st.ne.0)then
                write(0,'("Error: ",A," value must be integer")')key
                call exit(1)
              endif
              return
            endif
            read(nunit,ru_read_format,iostat=st)line
          end do
          if(present(nece))nece_=nece
          if(nece_)then
            write(0,'("Error: ",A," cannot find in input file")')key
            call exit(2)
          end if
          if(present(defval))then
            val=defval
          else
            val=defval_
          endif
          return
        end subroutine read_integer

        ! read integer values with key
        subroutine read_integers(nunit,key,val,nece,defval)
          integer,intent(in)                       :: nunit  ! unit number
          character(*),intent(in)                  :: key    ! key
          integer,dimension(:),intent(out)         :: val    ! return value
          logical,intent(in),optional              :: nece   ! necessity
          integer,dimension(:),intent(in),optional :: defval ! default value
          logical :: nece_=.true. ! default of nece
          integer :: defval_=0    ! default of defval
          character(ru_max_line_length) :: line
          character(ru_max_line_length) :: readkey
          character(ru_max_line_length) :: readval
          integer :: st
          integer :: err
          rewind(nunit)
          read(nunit,ru_read_format,iostat=st)line
          do while(st.eq.0)
            call trim_comment(line)
            line=adjustl(line)
            if (len_trim(line).eq.0) then
              read(nunit,ru_read_format,iostat=st)line
              cycle
            endif
            err=split_str(line,readkey,readval,"=")
            if((readkey.eq.key).and.(err.eq.0))then
              read(readval,*,iostat=st)val
              if(st.ne.0)then
                write(0,'("Error: ",A," value must be integer x ",i0)')
     *          key,size(val)
                call exit(1)
              endif
              return
            endif
            read(nunit,ru_read_format,iostat=st)line
          end do
          if(present(nece))nece_=nece
          if(nece_)then
            write(0,'("Error: ",A," cannot find in input file")')key
            call exit(2)
          end if
          if(present(defval))then
            val=defval
          else
            val=defval_
          endif
          return
        end subroutine read_integers

        ! read real*8 value with key
        subroutine read_real8(nunit,key,val,nece,defval)
          integer,intent(in)          :: nunit  ! unit number
          character(*),intent(in)     :: key    ! key
          real*8,intent(out)          :: val    ! return value
          logical,intent(in),optional :: nece   ! necessity
          real*8,intent(in),optional  :: defval ! default value
          logical :: nece_=.true. ! default of nece
          real*8  :: defval_=0.d0 ! default of defval
          character(ru_max_line_length) :: line
          character(ru_max_line_length) :: readkey
          character(ru_max_line_length) :: readval
          integer :: st
          integer :: err
          rewind(nunit)
          read(nunit,ru_read_format,iostat=st)line
          do while(st.eq.0)
            call trim_comment(line)
            line=adjustl(line)
            if (len_trim(line).eq.0) then
              read(nunit,ru_read_format,iostat=st)line
              cycle
            endif
            err=split_str(line,readkey,readval,"=")
            if((readkey.eq.key).and.(err.eq.0))then
              read(readval,*,iostat=st)val
              if(st.ne.0)then
                write(0,'("Error: ",A," value must be real*8")')key
                call exit(1)
              endif
              return
            endif
            read(nunit,ru_read_format,iostat=st)line
          end do
          if(present(nece))nece_=nece
          if(nece_)then
            write(0,'("Error: ",A," cannot find in input file")')key
            call exit(2)
          end if
          if(present(defval))then
            val=defval
          else
            val=defval_
          endif
          return
        end subroutine read_real8

        ! read real*8 values with key
        subroutine read_real8s(nunit,key,val,nece,defval)
          integer,intent(in)                      :: nunit  ! unit number
          character(*),intent(in)                 :: key    ! key
          real*8,dimension(:),intent(out)         :: val    ! return value
          logical,intent(in),optional             :: nece   ! necessity
          real*8,dimension(:),intent(in),optional :: defval ! default value
          logical :: nece_=.true. ! default of nece
          real*8  :: defval_=0.d0 ! default of defval
          character(ru_max_line_length) :: line
          character(ru_max_line_length) :: readkey
          character(ru_max_line_length) :: readval
          integer :: st
          integer :: err
          rewind(nunit)
          read(nunit,ru_read_format,iostat=st)line
          do while(st.eq.0)
            call trim_comment(line)
            line=adjustl(line)
            if (len_trim(line).eq.0) then
              read(nunit,ru_read_format,iostat=st)line
              cycle
            endif
            err=split_str(line,readkey,readval,"=")
            if((readkey.eq.key).and.(err.eq.0))then
              read(readval,*,iostat=st)val
              if(st.ne.0)then
                write(0,'("Error: ",A," value must be real*8 x ",i0)')
     *          key,size(val)
                call exit(1)
              endif
              return
            endif
            read(nunit,ru_read_format,iostat=st)line
          end do
          if(present(nece))nece_=nece
          if(nece_)then
            write(0,'("Error: ",A," cannot find in input file")')key
            call exit(2)
          end if
          if(present(defval))then
            val=defval
          else
            val=defval_
          endif
          return
        end subroutine read_real8s

        ! read string with key
        subroutine read_string(nunit,key,val,nece,defval)
          integer,intent(in)               :: nunit  ! unit number
          character(*),intent(in)          :: key    ! key
          character(*),intent(out)         :: val    ! return value
          logical,intent(in),optional      :: nece   ! necessity
          character(*),intent(in),optional :: defval ! default value
          logical :: nece_=.true.  ! default of nece
          character :: defval_=' ' ! default of defval
          character(ru_max_line_length) :: line
          character(ru_max_line_length) :: readkey
          integer :: st
          integer :: err
          rewind(nunit)
          read(nunit,ru_read_format,iostat=st)line
          do while(st.eq.0)
            call trim_comment(line)
            line=adjustl(line)
            if (len_trim(line).eq.0) then
              read(nunit,ru_read_format,iostat=st)line
              cycle
            endif
            err=split_str(line,readkey,val,"=")
            if((readkey.eq.key).and.(err.eq.0))then
              return
            endif
            read(nunit,ru_read_format,iostat=st)line
          end do
          if(present(nece))nece_=nece
          if(nece_)then
            write(0,'("Error: ",A," cannot find in input file")')key
            call exit(2)
          end if
          if(present(defval))then
            val=defval
          else
            val=defval_
          endif
          return
        end subroutine read_string

        ! trim comment
        subroutine trim_comment(str)
          character(*),intent(inout) :: str
          integer :: i
          do i=1,len_trim(str)
            if(str(i:i).eq.';')then
              str(i:len_trim(str))=' '
              exit
            end if
          end do
          return
        end subroutine trim_comment

        ! split string with delimiter
        integer function split_str(str,left,right,delimiter)
          character(*),intent(in)  :: str
          character(*),intent(out) :: left
          character(*),intent(out) :: right
          character(*),intent(in)  :: delimiter
          integer :: i
          do i=1,len_trim(str)
            if(str(i:i+len_trim(delimiter)-1).eq.delimiter)then
              left=str(1:i-1)
              right=str(i+1:len_trim(str))
              split_str=0
              return
            endif
          end do
          split_str=1
          return
        end function split_str

      end module
