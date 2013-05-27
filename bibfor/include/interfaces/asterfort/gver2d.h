        interface
          subroutine gver2d(noma,nocc,motfaz,nomno,noeud,rinf,rsup,&
     &module)
            character(len=8) :: noma
            integer :: nocc
            character(*) :: motfaz
            character(len=24) :: nomno
            character(len=8) :: noeud
            real(kind=8) :: rinf
            real(kind=8) :: rsup
            real(kind=8) :: module
          end subroutine gver2d
        end interface
