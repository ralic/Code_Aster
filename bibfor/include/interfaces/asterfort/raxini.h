        interface
          subroutine raxini(vsec1,vsec2,vsec3,vsec4,nptsec,nbordr,umin&
     &,umax,vmin,vmax,axeini)
            integer :: nbordr
            real(kind=8) :: vsec1(2*nbordr)
            real(kind=8) :: vsec2(2*nbordr)
            real(kind=8) :: vsec3(2*nbordr)
            real(kind=8) :: vsec4(2*nbordr)
            integer :: nptsec(4)
            real(kind=8) :: umin
            real(kind=8) :: umax
            real(kind=8) :: vmin
            real(kind=8) :: vmax
            character(len=4) :: axeini
          end subroutine raxini
        end interface
