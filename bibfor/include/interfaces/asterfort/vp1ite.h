        interface
          subroutine vp1ite(lmasse,lraide,ldynam,x,imode,valp,neq,&
     &mxiter,tol,iter,x0,mx,err,iexcl,place,iquoti,solveu)
            integer :: neq
            integer :: lmasse
            integer :: lraide
            integer :: ldynam
            real(kind=8) :: x(neq,1)
            integer :: imode
            real(kind=8) :: valp
            integer :: mxiter
            real(kind=8) :: tol
            integer :: iter
            real(kind=8) :: x0(neq)
            real(kind=8) :: mx(neq,*)
            real(kind=8) :: err
            integer :: iexcl(*)
            integer :: place
            integer :: iquoti
            character(len=19) :: solveu
          end subroutine vp1ite
        end interface
