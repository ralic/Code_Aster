        interface
          subroutine pjefmi(elrefp,nnop,coor,xg,ndim,x1,x2,lext,xmi)
            integer :: ndim
            integer :: nnop
            character(len=8) :: elrefp
            real(kind=8) :: coor(ndim*nnop)
            real(kind=8) :: xg(ndim)
            real(kind=8) :: x1(ndim)
            real(kind=8) :: x2(ndim)
            logical :: lext
            real(kind=8) :: xmi(ndim)
          end subroutine pjefmi
        end interface
