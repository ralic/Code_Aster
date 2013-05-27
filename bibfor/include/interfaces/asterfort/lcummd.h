        interface
          subroutine lcummd(vari,nvari,cmat,nmat,sigm,nstrs,isph,tdt,&
     &hini,hfin,an,bn,cn,cfps,cfpd)
            integer :: nmat
            integer :: nvari
            real(kind=8) :: vari(nvari)
            real(kind=8) :: cmat(nmat)
            real(kind=8) :: sigm(6)
            integer :: nstrs
            integer :: isph
            real(kind=8) :: tdt
            real(kind=8) :: hini
            real(kind=8) :: hfin
            real(kind=8) :: an(6)
            real(kind=8) :: bn(6,6)
            real(kind=8) :: cn(6,6)
            real(kind=8) :: cfps
            real(kind=8) :: cfpd
          end subroutine lcummd
        end interface
