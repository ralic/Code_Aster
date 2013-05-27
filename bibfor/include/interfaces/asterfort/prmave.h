        interface
          subroutine prmave(ipr,amat,na,na1,na2,bvec,nb1,cvec,nc1,ier)
            integer :: na
            integer :: ipr
            real(kind=8) :: amat(na,*)
            integer :: na1
            integer :: na2
            real(kind=8) :: bvec(*)
            integer :: nb1
            real(kind=8) :: cvec(*)
            integer :: nc1
            integer :: ier
          end subroutine prmave
        end interface
