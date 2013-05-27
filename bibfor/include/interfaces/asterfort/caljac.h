        interface
          subroutine caljac(np3,ic,typch,nbseg,chockc,rc,theta,vloc,&
     &xloc,vloc0,xloc0,tetaj,jacobc,jacobk)
            integer :: np3
            integer :: ic
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: chockc(*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: vloc(*)
            real(kind=8) :: xloc(*)
            real(kind=8) :: vloc0(*)
            real(kind=8) :: xloc0(*)
            real(kind=8) :: tetaj
            real(kind=8) :: jacobc(3,*)
            real(kind=8) :: jacobk(3,*)
          end subroutine caljac
        end interface
