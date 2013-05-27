        interface
          subroutine reerel(elrefp,nnop,ndim,tabar,xe,xg)
            integer :: ndim
            integer :: nnop
            character(len=8) :: elrefp
            real(kind=8) :: tabar(*)
            real(kind=8) :: xe(ndim)
            real(kind=8) :: xg(ndim)
          end subroutine reerel
        end interface
