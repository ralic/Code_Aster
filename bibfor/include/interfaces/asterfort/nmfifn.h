        interface
          subroutine nmfifn(nno,nddl,npg,wref,vff,dfde,geom,sigma,fint&
     &)
            integer :: npg
            integer :: nddl
            integer :: nno
            real(kind=8) :: wref(npg)
            real(kind=8) :: vff(nno,npg)
            real(kind=8) :: dfde(2,nno,npg)
            real(kind=8) :: geom(nddl)
            real(kind=8) :: sigma(3,npg)
            real(kind=8) :: fint(nddl)
          end subroutine nmfifn
        end interface
