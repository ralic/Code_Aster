        interface
          subroutine mahsf(ind1,nb1,xi,ksi3s2,intsn,xr,epais,vectn,&
     &vectg,vectt,hsf)
            integer :: ind1
            integer :: nb1
            real(kind=8) :: xi(3,*)
            real(kind=8) :: ksi3s2
            integer :: intsn
            real(kind=8) :: xr(*)
            real(kind=8) :: epais
            real(kind=8) :: vectn(9,3)
            real(kind=8) :: vectg(2,3)
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: hsf(3,9)
          end subroutine mahsf
        end interface
