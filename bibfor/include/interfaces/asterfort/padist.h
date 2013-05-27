        interface
          function padist(ndim,coor1,coor2)
            integer :: ndim
            real(kind=8) :: coor1(*)
            real(kind=8) :: coor2(*)
            real(kind=8) :: padist
          end function padist
        end interface
