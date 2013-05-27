        interface
          subroutine xproj(p,a,b,c,m,mp,d,vn,eps,in)
            real(kind=8) :: p(3)
            real(kind=8) :: a(3)
            real(kind=8) :: b(3)
            real(kind=8) :: c(3)
            real(kind=8) :: m(3)
            real(kind=8) :: mp(3)
            real(kind=8) :: d
            real(kind=8) :: vn(3)
            real(kind=8) :: eps(3)
            logical :: in
          end subroutine xproj
        end interface
