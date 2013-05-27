        interface
          subroutine matbmn(nb1,vectt,dudxnx,jdn1nx,jdn2nx,b1mnx,b2mnx&
     &)
            integer :: nb1
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: dudxnx(9)
            real(kind=8) :: jdn1nx(9,51)
            real(kind=8) :: jdn2nx(9,51)
            real(kind=8) :: b1mnx(3,51)
            real(kind=8) :: b2mnx(3,51)
          end subroutine matbmn
        end interface
