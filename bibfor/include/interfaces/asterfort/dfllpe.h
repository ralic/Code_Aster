        interface
          subroutine dfllpe(mcfact,iechec,even,penmax,nocham,nocmp,&
     &cricmp,valere)
            character(len=16) :: mcfact
            integer :: iechec
            character(len=16) :: even
            real(kind=8) :: penmax
            character(len=16) :: nocham
            character(len=16) :: nocmp
            character(len=16) :: cricmp
            real(kind=8) :: valere
          end subroutine dfllpe
        end interface
