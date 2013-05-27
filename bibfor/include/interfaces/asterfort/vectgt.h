        interface
          subroutine vectgt(ind,nb1,xi,ksi3s2,intsx,zr,epais,vectn,&
     &vectg,vectt)
            integer :: ind
            integer :: nb1
            real(kind=8) :: xi(3,*)
            real(kind=8) :: ksi3s2
            integer :: intsx
            real(kind=8) :: zr(*)
            real(kind=8) :: epais
            real(kind=8) :: vectn(9,3)
            real(kind=8) :: vectg(2,3)
            real(kind=8) :: vectt(3,3)
          end subroutine vectgt
        end interface
