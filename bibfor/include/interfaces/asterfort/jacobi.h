        interface
          subroutine jacobi(nbvec,nperm,tol,toldyn,ar,br,vecpro,valpro&
     &,valaux,nitjac,type,iordre)
            integer :: nbvec
            integer :: nperm
            real(kind=8) :: tol
            real(kind=8) :: toldyn
            real(kind=8) :: ar(*)
            real(kind=8) :: br(*)
            real(kind=8) :: vecpro(nbvec,nbvec)
            real(kind=8) :: valpro(nbvec)
            real(kind=8) :: valaux(nbvec)
            integer :: nitjac
            integer :: type
            integer :: iordre
          end subroutine jacobi
        end interface
