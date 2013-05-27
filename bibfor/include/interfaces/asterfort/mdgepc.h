        interface
          subroutine mdgepc(neq,nbmode,bmodal,xgene,u)
            integer :: nbmode
            integer :: neq
            real(kind=8) :: bmodal(neq,nbmode)
            complex(kind=8) :: xgene(nbmode)
            complex(kind=8) :: u(neq)
          end subroutine mdgepc
        end interface
