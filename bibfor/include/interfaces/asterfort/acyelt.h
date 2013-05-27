        interface
          subroutine acyelt(nmcolz,nomobz,nob,cmat,ndim,ideb,jdeb,x)
            character(*) :: nmcolz
            character(*) :: nomobz
            integer :: nob
            complex(kind=8) :: cmat(*)
            integer :: ndim
            integer :: ideb
            integer :: jdeb
            real(kind=8) :: x
          end subroutine acyelt
        end interface
