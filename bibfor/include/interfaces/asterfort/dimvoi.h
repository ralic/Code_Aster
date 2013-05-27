        interface
          subroutine dimvoi(nvtot,nvoima,nscoma,touvoi,dimvlo)
            integer :: nscoma
            integer :: nvoima
            integer :: nvtot
            integer :: touvoi(1:nvoima,1:nscoma+2)
            integer :: dimvlo
          end subroutine dimvoi
        end interface
