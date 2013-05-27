        interface
          subroutine lksige(mod,nmat,materd,deps,sigd,sigf)
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
          end subroutine lksige
        end interface
