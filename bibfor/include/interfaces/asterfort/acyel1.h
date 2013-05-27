        interface
          subroutine acyel1(nmcolz,nomobz,nobl,nobc,okpart,lilig,nblig&
     &,licol,nbcol,cmat,ndim,ideb,jdeb,x)
            integer :: ndim
            integer :: nbcol
            integer :: nblig
            character(*) :: nmcolz
            character(*) :: nomobz
            integer :: nobl
            integer :: nobc
            logical :: okpart
            integer :: lilig(nblig)
            integer :: licol(nbcol)
            complex(kind=8) :: cmat(ndim,ndim)
            integer :: ideb
            integer :: jdeb
            real(kind=8) :: x
          end subroutine acyel1
        end interface
