        interface
          subroutine nmini0(zpmet,zpcri,zconv,zpcon,znmeth,fonact,&
     &parmet,parcri,conv,parcon,method,eta,numins,matass,zmeelm,zmeass,&
     &zveelm,zveass,zsolal,zvalin,sdimpr)
            integer :: znmeth
            integer :: zpcon
            integer :: zconv
            integer :: zpcri
            integer :: zpmet
            integer :: fonact(*)
            real(kind=8) :: parmet(zpmet)
            real(kind=8) :: parcri(zpcri)
            real(kind=8) :: conv(zconv)
            real(kind=8) :: parcon(zpcon)
            character(len=16) :: method(znmeth)
            real(kind=8) :: eta
            integer :: numins
            character(len=19) :: matass
            integer :: zmeelm
            integer :: zmeass
            integer :: zveelm
            integer :: zveass
            integer :: zsolal
            integer :: zvalin
            character(len=24) :: sdimpr
          end subroutine nmini0
        end interface
