        interface
          subroutine lcumsd(vari,nvari,cmat,nmat,nstrs,isph,tdt,hini,&
     &hfin,afp,bfp,cfp,cfps,cfpd)
            integer :: nmat
            integer :: nvari
            real(kind=8) :: vari(nvari)
            real(kind=8) :: cmat(nmat)
            integer :: nstrs
            integer :: isph
            real(kind=8) :: tdt
            real(kind=8) :: hini
            real(kind=8) :: hfin
            real(kind=8) :: afp(6)
            real(kind=8) :: bfp(6,6)
            real(kind=8) :: cfp(6,6)
            real(kind=8) :: cfps
            real(kind=8) :: cfpd
          end subroutine lcumsd
        end interface
