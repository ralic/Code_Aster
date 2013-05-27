        interface
          subroutine cm27na(main,nbma,nbno,lima,typema,milieu,nomima,&
     &nomipe,mxnofa,nbhe20,nbtyma,deffac)
            integer :: nbtyma
            integer :: nbno
            integer :: nbma
            character(len=8) :: main
            integer :: lima(*)
            integer :: typema(*)
            integer :: milieu(4,24,nbno)
            integer :: nomima(6,nbma)
            integer :: nomipe(8,*)
            integer :: mxnofa
            integer :: nbhe20
            integer :: deffac(8,0:6,nbtyma)
          end subroutine cm27na
        end interface
