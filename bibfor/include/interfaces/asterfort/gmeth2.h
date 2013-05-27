        interface
          subroutine gmeth2(modele,nnoff,ndeg,chthet,fond,gthi,gs,&
     &objcur,xl,gi)
            character(len=8) :: modele
            integer :: nnoff
            integer :: ndeg
            character(len=24) :: chthet
            character(len=24) :: fond
            real(kind=8) :: gthi(1)
            real(kind=8) :: gs(1)
            character(len=24) :: objcur
            real(kind=8) :: xl
            real(kind=8) :: gi(1)
          end subroutine gmeth2
        end interface
