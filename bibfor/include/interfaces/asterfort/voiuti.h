        interface
          subroutine voiuti(numa,codvoi,nvoima,nscoma,iarepe,iaddvo,&
     &iadvoi,nbvois,livois,tyvois,nbnovo,nbsoco,lisoco)
            integer :: nscoma
            integer :: nvoima
            integer :: numa
            character(*) :: codvoi
            integer :: iarepe
            integer :: iaddvo
            integer :: iadvoi
            integer :: nbvois
            integer :: livois(1:nvoima)
            integer :: tyvois(1:nvoima)
            integer :: nbnovo(1:nvoima)
            integer :: nbsoco(1:nvoima)
            integer :: lisoco(1:nvoima,1:nscoma,1:2)
          end subroutine voiuti
        end interface
