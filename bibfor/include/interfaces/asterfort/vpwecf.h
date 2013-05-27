        interface
          subroutine vpwecf(option,typres,nfreq,mxfreq,resufi,resufr,&
     &resufk,lamor,ktyp,lns)
            integer :: mxfreq
            character(*) :: option
            character(*) :: typres
            integer :: nfreq
            integer :: resufi(mxfreq,*)
            real(kind=8) :: resufr(mxfreq,*)
            character(*) :: resufk(mxfreq,*)
            integer :: lamor
            character(len=1) :: ktyp
            logical :: lns
          end subroutine vpwecf
        end interface
