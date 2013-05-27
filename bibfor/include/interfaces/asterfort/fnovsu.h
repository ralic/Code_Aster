        interface
          subroutine fnovsu(option,nface,congem,vectu,press1,press2,&
     &dimcon,dimuel,typvf)
            integer :: dimuel
            integer :: dimcon
            character(len=16) :: option
            integer :: nface
            real(kind=8) :: congem(dimcon,7)
            real(kind=8) :: vectu(dimuel)
            integer :: press1(7)
            integer :: press2(7)
            integer :: typvf
          end subroutine fnovsu
        end interface
