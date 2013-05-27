        interface
          subroutine ftest2(np3,rc,theta,xloc,ic,typch,nbseg,dist2)
            integer :: np3
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: xloc(*)
            integer :: ic
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: dist2
          end subroutine ftest2
        end interface
