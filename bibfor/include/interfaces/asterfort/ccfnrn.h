        interface
          subroutine ccfnrn(option,resuin,resuou,lisord,nbordr,lischa,&
     &ncharg,chtype,typesd)
            character(len=16) :: option
            character(len=8) :: resuin
            character(len=8) :: resuou
            character(len=19) :: lisord
            integer :: nbordr
            character(len=19) :: lischa
            integer :: ncharg
            character(len=4) :: chtype
            character(len=16) :: typesd
          end subroutine ccfnrn
        end interface
