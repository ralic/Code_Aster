        interface
          function dvolu1(numele,coord,norm,volt)
            integer :: numele
            real(kind=8) :: coord(3,12)
            integer :: norm(2,4)
            real(kind=8) :: volt(*)
            real(kind=8) :: dvolu1
          end function dvolu1
        end interface
