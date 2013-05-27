        interface
          function hdfwat(i,knom1,j,knom2)
            integer :: j
            integer :: i
            character(*) :: knom1
            character(*) :: knom2(j)
            integer :: hdfwat
          end function hdfwat
        end interface
