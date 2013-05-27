        interface
          subroutine gmeth4(nnoff,ndimte,fond,gthi,milieu,pair,gs,&
     &objcur,gi,gxfem)
            integer :: nnoff
            integer :: ndimte
            character(len=24) :: fond
            real(kind=8) :: gthi(1)
            logical :: milieu
            logical :: pair
            real(kind=8) :: gs(1)
            character(len=24) :: objcur
            real(kind=8) :: gi(1)
            logical :: gxfem
          end subroutine gmeth4
        end interface
