        interface
          subroutine btdfn(ind,nb1,nb2,ksi3s2,intsn,xr,epais,vectpt,&
     &hsj1fx,btdf)
            integer :: ind
            integer :: nb1
            integer :: nb2
            real(kind=8) :: ksi3s2
            integer :: intsn
            real(kind=8) :: xr(*)
            real(kind=8) :: epais
            real(kind=8) :: vectpt(9,2,3)
            real(kind=8) :: hsj1fx(3,9)
            real(kind=8) :: btdf(3,42)
          end subroutine btdfn
        end interface
