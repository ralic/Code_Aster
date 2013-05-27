        interface
          subroutine forsrg(intsn,nb1,nb2,xr,chgsrg,rnormc,vectpt,&
     &vecl1)
            integer :: intsn
            integer :: nb1
            integer :: nb2
            real(kind=8) :: xr(*)
            real(kind=8) :: chgsrg(6,8)
            real(kind=8) :: rnormc
            real(kind=8) :: vectpt(9,3,3)
            real(kind=8) :: vecl1(42)
          end subroutine forsrg
        end interface
