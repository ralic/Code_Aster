        interface
          subroutine vptabl(tabmod,typevp,fmin,fmax,precdc,nfreq,&
     &effmin,effmax)
            character(len=19) :: tabmod
            character(len=9) :: typevp
            real(kind=8) :: fmin
            real(kind=8) :: fmax
            real(kind=8) :: precdc
            integer :: nfreq
            real(kind=8) :: effmin
            real(kind=8) :: effmax
          end subroutine vptabl
        end interface
