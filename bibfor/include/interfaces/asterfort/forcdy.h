        interface
          subroutine forcdy(masse,amort,lamort,neq,c0,c1,c2,c3,c4,c5,&
     &d0,v0,a0,f1,f2,f)
            integer :: masse
            integer :: amort
            logical :: lamort
            integer :: neq
            real(kind=8) :: c0
            real(kind=8) :: c1
            real(kind=8) :: c2
            real(kind=8) :: c3
            real(kind=8) :: c4
            real(kind=8) :: c5
            real(kind=8) :: d0(*)
            real(kind=8) :: v0(*)
            real(kind=8) :: a0(*)
            real(kind=8) :: f1(*)
            real(kind=8) :: f2(*)
            real(kind=8) :: f(*)
          end subroutine forcdy
        end interface
