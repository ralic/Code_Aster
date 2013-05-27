        interface
          subroutine vechmp(nomo,mate,carele,varplu,lxfem,partps,nbin,&
     &lpain,lchin,lastin)
            integer :: nbin
            character(len=8) :: nomo
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: varplu
            logical :: lxfem
            real(kind=8) :: partps(3)
            character(len=8) :: lpain(nbin)
            character(len=19) :: lchin(nbin)
            integer :: lastin
          end subroutine vechmp
        end interface
