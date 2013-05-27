        interface
          subroutine caltol(np3,nbnl,typch,nbseg,rc,theta,tol,tolc,&
     &toln,tolv)
            integer :: np3
            integer :: nbnl
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: tol
            real(kind=8) :: tolc
            real(kind=8) :: toln
            real(kind=8) :: tolv
          end subroutine caltol
        end interface
