        interface
          subroutine vpqlts(diag,surdia,neq,vecpro,mxcmp,mxiter,ier,&
     &nitqr)
            integer :: mxcmp
            real(kind=8) :: diag(1)
            real(kind=8) :: surdia(1)
            integer :: neq
            real(kind=8) :: vecpro(mxcmp,1)
            integer :: mxiter
            integer :: ier
            integer :: nitqr
          end subroutine vpqlts
        end interface
