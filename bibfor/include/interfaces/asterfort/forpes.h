        interface
          subroutine forpes(intsn,nb1,xr,rho,epais,vpesan,rnormc,vecl1&
     &)
            integer :: intsn
            integer :: nb1
            real(kind=8) :: xr(*)
            real(kind=8) :: rho
            real(kind=8) :: epais
            real(kind=8) :: vpesan(3)
            real(kind=8) :: rnormc
            real(kind=8) :: vecl1(42)
          end subroutine forpes
        end interface
