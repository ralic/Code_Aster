        interface
          subroutine crvloc(dim,adcom0,iatyma,connex,vgeloc,nvtot,&
     &nvoima,nscoma,touvoi)
            integer :: nscoma
            integer :: nvoima
            integer :: dim
            integer :: adcom0
            integer :: iatyma
            character(len=24) :: connex
            integer :: vgeloc(*)
            integer :: nvtot
            integer :: touvoi(1:nvoima,1:nscoma+2)
          end subroutine crvloc
        end interface
