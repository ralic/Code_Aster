        interface
          subroutine dxmate(fami,df,dm,dmf,dc,dci,dmc,dfc,nno,pgl,&
     &multic,coupmf,t2ev,t2ve,t1ve)
            character(len=4) :: fami
            real(kind=8) :: df(3,3)
            real(kind=8) :: dm(3,3)
            real(kind=8) :: dmf(3,3)
            real(kind=8) :: dc(2,2)
            real(kind=8) :: dci(2,2)
            real(kind=8) :: dmc(3,2)
            real(kind=8) :: dfc(3,2)
            integer :: nno
            real(kind=8) :: pgl(3,3)
            integer :: multic
            logical :: coupmf
            real(kind=8) :: t2ev(4)
            real(kind=8) :: t2ve(4)
            real(kind=8) :: t1ve(9)
          end subroutine dxmate
        end interface
