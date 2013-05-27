        interface
          subroutine mdgeph(neq,nbmode,bmodal,xgene,u)
            integer :: nbmode
            integer :: neq
            real(kind=8) :: bmodal(neq,nbmode)
            real(kind=8) :: xgene(nbmode)
            real(kind=8) :: u(neq)
          end subroutine mdgeph
        end interface
