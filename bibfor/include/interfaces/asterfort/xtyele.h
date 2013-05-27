        interface
          subroutine xtyele(noma,trav,nfiss,fiss,contac,ndim,linter)
            integer :: ndim
            integer :: nfiss
            character(len=8) :: noma
            character(len=24) :: trav
            character(len=8) :: fiss(nfiss)
            integer :: contac
            logical :: linter
          end subroutine xtyele
        end interface
