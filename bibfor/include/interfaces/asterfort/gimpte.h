        interface
          subroutine gimpte(resu,rayinf,raysup,theta,nomnoe,dir,absc,&
     &nbno,format,unit)
            character(len=8) :: resu
            real(kind=8) :: rayinf(*)
            real(kind=8) :: raysup(*)
            real(kind=8) :: theta(*)
            character(len=8) :: nomnoe(*)
            real(kind=8) :: dir(*)
            real(kind=8) :: absc(*)
            integer :: nbno
            character(*) :: format
            integer :: unit
          end subroutine gimpte
        end interface
