        interface
          function ioriv3(num,noeud,vect,coor)
            integer :: num(2)
            integer :: noeud
            real(kind=8) :: vect(3)
            real(kind=8) :: coor(3,*)
            integer :: ioriv3
          end function ioriv3
        end interface
