        interface
          function ioriv2(num,n,noeud,vect,coor)
            integer :: n
            integer :: num(n)
            integer :: noeud
            real(kind=8) :: vect(3)
            real(kind=8) :: coor(3,*)
            integer :: ioriv2
          end function ioriv2
        end interface
