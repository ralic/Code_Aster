        interface
          subroutine mdgep5(neq,nbmode,bmodal,xgene,iddl,u)
            integer :: neq
            integer :: nbmode
            real(kind=8) :: bmodal(neq,*)
            complex(kind=8) :: xgene(*)
            integer :: iddl
            complex(kind=8) :: u
          end subroutine mdgep5
        end interface
