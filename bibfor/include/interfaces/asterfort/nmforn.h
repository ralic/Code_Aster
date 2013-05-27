        interface
          subroutine nmforn(ndim,nno1,nno2,npg,iw,vff1,vff2,idfde1,&
     &geom,vect)
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: ndim
            integer :: iw
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            integer :: idfde1
            real(kind=8) :: geom(ndim,nno1)
            real(kind=8) :: vect(*)
          end subroutine nmforn
        end interface
