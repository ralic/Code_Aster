        interface
          subroutine calcfo(compl,nomfin,nomfon,nbval,vale,nopara)
            logical :: compl
            character(len=19) :: nomfin
            character(len=19) :: nomfon
            integer :: nbval
            real(kind=8) :: vale(*)
            character(len=24) :: nopara
          end subroutine calcfo
        end interface
