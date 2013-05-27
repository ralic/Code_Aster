        interface
          subroutine lcelin(mod,nmat,materd,materf,deps,sigd,sigf)
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
          end subroutine lcelin
        end interface
