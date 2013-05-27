        interface
          subroutine cm18na(main,nbma,nbno,lima,typema,milieu,nomima,&
     &nomipe,mxnofa,nbtyma,deffac)
            integer :: nbtyma
            integer :: nbno
            integer :: nbma
            character(len=8) :: main
            integer :: lima(*)
            integer :: typema(*)
            integer :: milieu(4,24,nbno)
            integer :: nomima(3,nbma)
            integer :: nomipe(8,*)
            integer :: mxnofa
            integer :: deffac(8,0:6,nbtyma)
          end subroutine cm18na
        end interface
