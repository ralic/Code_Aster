        interface
          subroutine matbsr(nb1,vectt,dudxrc,intsr,jdn1rc,jdn2rc,b1src&
     &,b2src)
            integer :: nb1
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: dudxrc(9)
            integer :: intsr
            real(kind=8) :: jdn1rc(9,51)
            real(kind=8) :: jdn2rc(9,51)
            real(kind=8) :: b1src(2,51,4)
            real(kind=8) :: b2src(2,51,4)
          end subroutine matbsr
        end interface
