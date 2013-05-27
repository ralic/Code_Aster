        interface
          subroutine rechmc(ndim,temps,oridef,tabrev,tabmdb,norev,&
     &sigmrv,nomdb,sigmdb)
            integer :: ndim
            real(kind=8) :: temps
            character(len=8) :: oridef
            character(len=8) :: tabrev
            character(len=8) :: tabmdb
            integer :: norev
            character(len=19) :: sigmrv
            integer :: nomdb
            character(len=19) :: sigmdb
          end subroutine rechmc
        end interface
