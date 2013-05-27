        interface
          function hdftyp(i,knom1,j,knom2)
            integer :: j
            integer :: i
            character(*) :: knom1
            character(*) :: knom2(j)
            integer :: hdftyp
          end function hdftyp
        end interface
