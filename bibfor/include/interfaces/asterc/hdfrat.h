        interface
          function hdfrat(i,knom1,j,knom2)
            integer :: j
            integer :: i
            character(*) :: knom1
            character(*) :: knom2(j)
            integer :: hdfrat
          end function hdfrat
        end interface
