        interface
          subroutine acyel4(nmcolz,nomobz,nobl,nobc,okpart,lilig,nblig&
     &,licol,nbcol,cmat,ndim,ideb,jdeb,beta)
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
            real(kind=8) :: beta
          end subroutine acyel4
        end interface
