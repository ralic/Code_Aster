        interface
          subroutine calcgr(igau,nbsig,nbvari,vip,nu,epsfl)
            integer :: nbsig
            integer :: igau
            integer :: nbvari
            real(kind=8) :: vip(*)
            real(kind=8) :: nu
            real(kind=8) :: epsfl(nbsig)
          end subroutine calcgr
        end interface
