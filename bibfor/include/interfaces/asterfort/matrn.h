        interface
          subroutine matrn(nb1,nb2,xr,ksi3s2,epais,intsn,vectn,matn)
            integer :: nb1
            integer :: nb2
            real(kind=8) :: xr(*)
            real(kind=8) :: ksi3s2
            real(kind=8) :: epais
            integer :: intsn
            real(kind=8) :: vectn(9,3)
            real(kind=8) :: matn(3,51)
          end subroutine matrn
        end interface
