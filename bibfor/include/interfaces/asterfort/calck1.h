        interface
          subroutine calck1(norev,nomdb,sigmrv,sigmdb,tbscrv,tbscmb,&
     &prodef,londef,deklag,lrev,k1a,k1b)
            integer :: norev
            integer :: nomdb
            character(len=19) :: sigmrv
            character(len=19) :: sigmdb
            character(len=19) :: tbscrv
            character(len=19) :: tbscmb
            real(kind=8) :: prodef
            real(kind=8) :: londef
            real(kind=8) :: deklag
            real(kind=8) :: lrev
            real(kind=8) :: k1a
            real(kind=8) :: k1b
          end subroutine calck1
        end interface
