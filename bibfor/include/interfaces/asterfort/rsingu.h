        interface
          subroutine rsingu(ndim,nelem,nbr,nalpha,degre,prec,erreur,&
     &alpha,types,re)
            integer :: nelem
            integer :: ndim
            integer :: nbr(nelem)
            integer :: nalpha
            integer :: degre
            real(kind=8) :: prec
            real(kind=8) :: erreur(nelem)
            real(kind=8) :: alpha(nelem)
            character(len=16) :: types
            real(kind=8) :: re(nelem)
          end subroutine rsingu
        end interface
