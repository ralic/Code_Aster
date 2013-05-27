        interface
          subroutine nifnpd(ndim,nno1,nno2,nno3,npg,iw,vff1,vff2,vff3,&
     &idff1,vu,vg,vp,typmod,geomi,sig,ddl,vect)
            integer :: npg
            integer :: nno3
            integer :: nno2
            integer :: nno1
            integer :: ndim
            integer :: iw
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            real(kind=8) :: vff3(nno3,npg)
            integer :: idff1
            integer :: vu(3,27)
            integer :: vg(27)
            integer :: vp(27)
            character(len=8) :: typmod(*)
            real(kind=8) :: geomi(ndim,nno1)
            real(kind=8) :: sig(2*ndim+1,npg)
            real(kind=8) :: ddl(*)
            real(kind=8) :: vect(*)
          end subroutine nifnpd
        end interface
