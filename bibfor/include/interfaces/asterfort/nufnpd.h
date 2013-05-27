        interface
          subroutine nufnpd(ndim,nno1,nno2,npg,iw,vff1,vff2,idff1,vu,&
     &vp,typmod,mate,compor,geomi,sig,ddl,mini,vect)
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
            integer :: mate
            character(len=16) :: compor(4)
            real(kind=8) :: geomi(ndim,nno1)
            real(kind=8) :: sig(2*ndim+1,npg)
            real(kind=8) :: ddl(*)
            logical :: mini
            real(kind=8) :: vect(*)
          end subroutine nufnpd
        end interface
