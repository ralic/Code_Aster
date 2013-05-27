        interface
          subroutine nmepsb(ndim,nno,axi,vff,dfdi,deplg,epsb,geps)
            integer :: nno
            integer :: ndim
            logical :: axi
            real(kind=8) :: vff(nno)
            real(kind=8) :: dfdi(nno,ndim)
            real(kind=8) :: deplg(*)
            real(kind=8) :: epsb(6)
            real(kind=8) :: geps(6,3)
          end subroutine nmepsb
        end interface
