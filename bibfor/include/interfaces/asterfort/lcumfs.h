        interface
          subroutine lcumfs(vari,nvari,cmat,nmat,iflu,isph,tdt,hini,&
     &hfin,afps,bfps,cfps)
            integer :: nmat
            integer :: nvari
            real(kind=8) :: vari(nvari)
            real(kind=8) :: cmat(nmat)
            integer :: iflu
            integer :: isph
            real(kind=8) :: tdt
            real(kind=8) :: hini
            real(kind=8) :: hfin
            real(kind=8) :: afps
            real(kind=8) :: bfps
            real(kind=8) :: cfps
          end subroutine lcumfs
        end interface
