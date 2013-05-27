        interface
          subroutine sspace(lraid,lmatra,lmass,neq,nbvec,nfreq,lprod,&
     &itemax,nperm,tol,toldyn,vect,valpro,nitjac,nitbat,solveu)
            integer :: nbvec
            integer :: neq
            integer :: lraid
            integer :: lmatra
            integer :: lmass
            integer :: nfreq
            integer :: lprod(neq)
            integer :: itemax
            integer :: nperm
            real(kind=8) :: tol
            real(kind=8) :: toldyn
            real(kind=8) :: vect(neq,nbvec)
            real(kind=8) :: valpro(nbvec)
            integer :: nitjac
            integer :: nitbat
            character(len=19) :: solveu
          end subroutine sspace
        end interface
