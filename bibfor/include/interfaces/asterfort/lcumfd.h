        interface
          subroutine lcumfd(vari,nvari,nstrs,cmat,nmat,iflu,tdt,hini,&
     &hfin,afpd,bfpd,cfpd)
            integer :: nmat
            integer :: nvari
            real(kind=8) :: vari(nvari)
            integer :: nstrs
            real(kind=8) :: cmat(nmat)
            integer :: iflu
            real(kind=8) :: tdt
            real(kind=8) :: hini
            real(kind=8) :: hfin
            real(kind=8) :: afpd(6)
            real(kind=8) :: bfpd
            real(kind=8) :: cfpd
          end subroutine lcumfd
        end interface
