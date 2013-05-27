        interface
          subroutine gmeth3(nnoff,fond,gthi,milieu,gs,objcur,gi,num,&
     &gxfem)
            integer :: nnoff
            character(len=24) :: fond
            real(kind=8) :: gthi(1)
            logical :: milieu
            real(kind=8) :: gs(1)
            character(len=24) :: objcur
            real(kind=8) :: gi(1)
            integer :: num
            logical :: gxfem
          end subroutine gmeth3
        end interface
