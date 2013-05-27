        interface
          subroutine mdgep2(neq,nbmode,bmodal,xgene,iddl,u)
            integer :: neq
            integer :: nbmode
            real(kind=8) :: bmodal(neq,*)
            real(kind=8) :: xgene(*)
            integer :: iddl
            real(kind=8) :: u
          end subroutine mdgep2
        end interface
