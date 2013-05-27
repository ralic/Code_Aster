        interface
          subroutine promat(a,nlamax,dimal,dimac,b,nlbmax,dimbl,dimbc,&
     &res)
            integer :: nlbmax
            integer :: nlamax
            real(kind=8) :: a(nlamax,*)
            integer :: dimal
            integer :: dimac
            real(kind=8) :: b(nlbmax,*)
            integer :: dimbl
            integer :: dimbc
            real(kind=8) :: res(nlamax,*)
          end subroutine promat
        end interface
