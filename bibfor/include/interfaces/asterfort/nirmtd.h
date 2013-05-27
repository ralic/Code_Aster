        interface
          subroutine nirmtd(ndim,nno1,nno2,nno3,npg,iw,vff2,vff3,ivf1,&
     &idff1,vu,vg,vp,igeom,mate,matr)
            integer :: npg
            integer :: nno3
            integer :: nno2
            integer :: nno1
            integer :: ndim
            integer :: iw
            real(kind=8) :: vff2(nno2,npg)
            real(kind=8) :: vff3(nno3,npg)
            integer :: ivf1
            integer :: idff1
            integer :: vu(3,27)
            integer :: vg(27)
            integer :: vp(27)
            integer :: igeom
            integer :: mate
            real(kind=8) :: matr(*)
          end subroutine nirmtd
        end interface
