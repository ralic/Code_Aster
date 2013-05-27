        interface
          subroutine nmfogn(ndim,nno1,nno2,npg,iw,vff1,vff2,idfde1,&
     &idfde2,geom,typmod,mat,ddl,sigm,vect)
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: ndim
            integer :: iw
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            integer :: idfde1
            integer :: idfde2
            real(kind=8) :: geom(ndim,nno1)
            character(len=8) :: typmod(*)
            integer :: mat
            real(kind=8) :: ddl(*)
            real(kind=8) :: sigm(2*ndim+1,npg)
            real(kind=8) :: vect(*)
          end subroutine nmfogn
        end interface
