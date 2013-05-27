        interface
          subroutine vphqrp(mat,neq,mxeq,icode,w,z,iz,wk,mxiter,ier,&
     &nitqr)
            integer :: mxeq
            integer :: neq
            real(kind=8) :: mat(mxeq,1)
            integer :: icode
            real(kind=8) :: w(1)
            real(kind=8) :: z(1)
            integer :: iz
            real(kind=8) :: wk(neq,1)
            integer :: mxiter
            integer :: ier
            integer :: nitqr
          end subroutine vphqrp
        end interface
