        interface
          subroutine fgdohs(nommat,nbcycl,sigmin,sigmax,lke,rke,lhaigh&
     &,rcorr,dom)
            character(len=8) :: nommat
            integer :: nbcycl
            real(kind=8) :: sigmin(*)
            real(kind=8) :: sigmax(*)
            logical :: lke
            real(kind=8) :: rke(*)
            logical :: lhaigh
            real(kind=8) :: rcorr(*)
            real(kind=8) :: dom(*)
          end subroutine fgdohs
        end interface
