        interface
          subroutine fetsco(nbmato,nblien,connec,idconn,nbpart,mapsd,&
     &edlo,velo,numver,ier)
            integer :: nbmato
            integer :: nblien
            integer(kind=4) :: connec
            integer(kind=4) :: idconn
            integer :: nbpart
            integer(kind=4) :: mapsd
            integer(kind=4) :: edlo
            integer(kind=4) :: velo
            integer :: numver
            integer :: ier
          end subroutine fetsco
        end interface
