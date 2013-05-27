        interface
          subroutine pmconv(r,rini,r1,inst,sigp,coef,iter,indimp,&
     &parcri,conver,itemax)
            real(kind=8) :: r(12)
            real(kind=8) :: rini(12)
            real(kind=8) :: r1(12)
            real(kind=8) :: inst
            real(kind=8) :: sigp(6)
            real(kind=8) :: coef
            integer :: iter
            integer :: indimp(6)
            real(kind=8) :: parcri(*)
            logical :: conver
            logical :: itemax
          end subroutine pmconv
        end interface
