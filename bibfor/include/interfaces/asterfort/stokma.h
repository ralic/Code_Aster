        interface
          subroutine stokma(amat,nlig,ncol,nmat,amatst)
            integer :: ncol
            integer :: nlig
            real(kind=8) :: amat(nlig,ncol)
            integer :: nmat
            real(kind=8) :: amatst(9,6,6)
          end subroutine stokma
        end interface
