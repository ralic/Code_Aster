        interface
          subroutine nurfpd(ndim,nno1,nno2,npg,iw,vff1,vff2,idff1,vu,&
     &vp,typmod,geomi,sigref,epsref,vect)
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: ndim
            integer :: iw
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            integer :: idff1
            integer :: vu(3,27)
            integer :: vp(27)
            character(len=8) :: typmod(*)
            real(kind=8) :: geomi(ndim,nno1)
            real(kind=8) :: sigref
            real(kind=8) :: epsref
            real(kind=8) :: vect(*)
          end subroutine nurfpd
        end interface
