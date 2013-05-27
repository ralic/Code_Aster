        interface
          subroutine vpdich(lraide,lmasse,ldynam,tol,mxdich,mxfreq,&
     &nfreq,valp,ieme,det,idet,nbpas,typres,nblagr,solveu)
            integer :: lraide
            integer :: lmasse
            integer :: ldynam
            real(kind=8) :: tol
            integer :: mxdich
            integer :: mxfreq
            integer :: nfreq
            real(kind=8) :: valp(*)
            integer :: ieme(*)
            real(kind=8) :: det(*)
            integer :: idet(*)
            integer :: nbpas(*)
            character(len=16) :: typres
            integer :: nblagr
            character(len=19) :: solveu
          end subroutine vpdich
        end interface
