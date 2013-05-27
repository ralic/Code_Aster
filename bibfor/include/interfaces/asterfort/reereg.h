        interface
          subroutine reereg(stop,elrefp,nnop,coor,xg,ndim,xe,iret)
            integer :: ndim
            integer :: nnop
            character(len=1) :: stop
            character(len=8) :: elrefp
            real(kind=8) :: coor(ndim*nnop)
            real(kind=8) :: xg(ndim)
            real(kind=8) :: xe(ndim)
            integer :: iret
          end subroutine reereg
        end interface
