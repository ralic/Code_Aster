        interface
          subroutine fgdomg(method,nommat,nomnap,nomfon,valmin,valmax,&
     &ncyc,dommag)
            character(*) :: method
            character(len=8) :: nommat
            character(len=8) :: nomnap
            character(len=8) :: nomfon
            real(kind=8) :: valmin(*)
            real(kind=8) :: valmax(*)
            integer :: ncyc
            real(kind=8) :: dommag
          end subroutine fgdomg
        end interface
