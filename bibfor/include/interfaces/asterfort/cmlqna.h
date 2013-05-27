        interface
          subroutine cmlqna(nbma,nbno,lima,connez,typema,mxar,milieu,&
     &nomima,nomipe,mxnomi,nbtyma,defare)
            integer :: nbtyma
            integer :: mxar
            integer :: nbno
            integer :: nbma
            integer :: lima(*)
            character(*) :: connez
            integer :: typema(*)
            integer :: milieu(2,mxar,nbno)
            integer :: nomima(12,nbma)
            integer :: nomipe(2,*)
            integer :: mxnomi
            integer :: defare(2,0:12,nbtyma)
          end subroutine cmlqna
        end interface
