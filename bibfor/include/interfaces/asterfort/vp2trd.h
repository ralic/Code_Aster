        interface
          subroutine vp2trd(type,nbvect,alpha,beta,signes,vecpro,&
     &mxiter,nitqr)
            character(len=1) :: type
            integer :: nbvect
            real(kind=8) :: alpha(*)
            real(kind=8) :: beta(*)
            real(kind=8) :: signes(*)
            real(kind=8) :: vecpro(*)
            integer :: mxiter
            integer :: nitqr
          end subroutine vp2trd
        end interface
