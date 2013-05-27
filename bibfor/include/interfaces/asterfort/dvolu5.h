        interface
          function dvolu5(numele,coord,norm,volt,coord1,coord2)
            integer :: numele
            real(kind=8) :: coord(3,12)
            integer :: norm(2,4)
            real(kind=8) :: volt(*)
            real(kind=8) :: coord1(3)
            real(kind=8) :: coord2(3)
            real(kind=8) :: dvolu5
          end function dvolu5
        end interface
