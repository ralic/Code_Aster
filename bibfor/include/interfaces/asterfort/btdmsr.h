        interface
          subroutine btdmsr(nb1,nb2,ksi3s2,intsr,xr,epais,vectpt,hsj1m&
     &,hsj1s,btdm,btds)
            integer :: nb1
            integer :: nb2
            real(kind=8) :: ksi3s2
            integer :: intsr
            real(kind=8) :: xr(*)
            real(kind=8) :: epais
            real(kind=8) :: vectpt(9,2,3)
            real(kind=8) :: hsj1m(3,9)
            real(kind=8) :: hsj1s(2,9)
            real(kind=8) :: btdm(4,3,42)
            real(kind=8) :: btds(4,2,42)
          end subroutine btdmsr
        end interface
