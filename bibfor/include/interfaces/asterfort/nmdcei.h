        interface
          subroutine nmdcei(sddisc,numins,newins,nbini,nbins,typext,&
     &dt0)
            integer :: nbins
            character(len=19) :: sddisc
            integer :: numins
            real(kind=8) :: newins(nbins)
            integer :: nbini
            character(len=4) :: typext
            real(kind=8) :: dt0
          end subroutine nmdcei
        end interface
