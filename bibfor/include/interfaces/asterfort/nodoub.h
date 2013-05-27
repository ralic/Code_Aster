        interface
          subroutine nodoub(nbl,nbb,nol,nob,typl,typb,mailla,double)
            integer :: nbb
            integer :: nbl
            integer :: nol(nbl)
            integer :: nob(nbb)
            character(len=8) :: typl
            character(len=8) :: typb
            character(len=8) :: mailla
            logical :: double
          end subroutine nodoub
        end interface
