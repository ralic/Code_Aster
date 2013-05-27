        interface
          subroutine lcumfb(sigi,nstrs,vari,nvari,cmat,nmat,tdt,hini,&
     &hfin,afd,bfd,cfd)
            integer :: nmat
            integer :: nvari
            real(kind=8) :: sigi(6)
            integer :: nstrs
            real(kind=8) :: vari(nvari)
            real(kind=8) :: cmat(nmat)
            real(kind=8) :: tdt
            real(kind=8) :: hini
            real(kind=8) :: hfin
            real(kind=8) :: afd(6)
            real(kind=8) :: bfd(6,6)
            real(kind=8) :: cfd(6,6)
          end subroutine lcumfb
        end interface
