        interface
          subroutine nurmtd(ndim,nno1,nno2,npg,iw,vff1,vff2,ivf1,idff1&
     &,vu,vp,typmod,igeom,mate,mini,matr)
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: ndim
            integer :: iw
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            integer :: ivf1
            integer :: idff1
            integer :: vu(3,27)
            integer :: vp(27)
            character(len=8) :: typmod(*)
            integer :: igeom
            integer :: mate
            logical :: mini
            real(kind=8) :: matr(*)
          end subroutine nurmtd
        end interface
