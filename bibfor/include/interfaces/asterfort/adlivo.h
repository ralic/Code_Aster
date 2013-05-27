        interface
          subroutine adlivo(mv,is,nvtot,nvoima,nscoma,touvoi)
            integer :: nscoma
            integer :: nvoima
            integer :: mv
            integer :: is
            integer :: nvtot
            integer :: touvoi(1:nvoima,1:nscoma+2)
          end subroutine adlivo
        end interface
