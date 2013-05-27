        interface
          subroutine fgdowh(nommat,nbcycl,sigmin,sigmax,lke,rke,lhaigh&
     &,rcorr,dom)
            character(*) :: nommat
            integer :: nbcycl
            real(kind=8) :: sigmin(*)
            real(kind=8) :: sigmax(*)
            logical :: lke
            real(kind=8) :: rke(*)
            logical :: lhaigh
            real(kind=8) :: rcorr(*)
            real(kind=8) :: dom(*)
          end subroutine fgdowh
        end interface
