        interface
          subroutine extrma(amatst,nlig,ncol,nmat,amat)
            integer :: ncol
            integer :: nlig
            real(kind=8) :: amatst(9,6,6)
            integer :: nmat
            real(kind=8) :: amat(nlig,ncol)
          end subroutine extrma
        end interface
