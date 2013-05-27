        interface
          subroutine ejfore(ndim,nddl,axi,nno1,nno2,npg,ipg,wref,vff1,&
     &vff2,idf2,dffr2,geom,iu,ip,sigref,fhyref,vect)
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: nddl
            integer :: ndim
            logical :: axi
            integer :: ipg
            real(kind=8) :: wref(npg)
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            integer :: idf2
            real(kind=8) :: dffr2(ndim-1,nno2,npg)
            real(kind=8) :: geom(ndim,nno2)
            integer :: iu(3,16)
            integer :: ip(4)
            real(kind=8) :: sigref
            real(kind=8) :: fhyref
            real(kind=8) :: vect(nddl)
          end subroutine ejfore
        end interface
