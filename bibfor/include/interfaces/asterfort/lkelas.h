        interface
          subroutine lkelas(ndi,ndt,mod,nmat,mater,deps,sigd,de,k,mu)
            integer :: nmat
            integer :: ndi
            integer :: ndt
            character(len=8) :: mod
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: de(6,6)
            real(kind=8) :: k
            real(kind=8) :: mu
          end subroutine lkelas
        end interface
