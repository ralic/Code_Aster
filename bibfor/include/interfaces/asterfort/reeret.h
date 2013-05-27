        interface
          subroutine reeret(elrefp,nnop,geom,xg,ndim,deriv,xe,ff,dfdi)
            integer :: ndim
            integer :: nnop
            character(len=8) :: elrefp
            real(kind=8) :: geom(*)
            real(kind=8) :: xg(ndim)
            character(len=3) :: deriv
            real(kind=8) :: xe(ndim)
            real(kind=8) :: ff(nnop)
            real(kind=8) :: dfdi(nnop,ndim)
          end subroutine reeret
        end interface
