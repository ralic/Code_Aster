        interface
          function ioriv1(num,noeud,vect,coor)
            integer :: num(2)
            integer :: noeud
            real(kind=8) :: vect(2)
            real(kind=8) :: coor(3,*)
            integer :: ioriv1
          end function ioriv1
        end interface
