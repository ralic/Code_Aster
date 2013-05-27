        interface
          subroutine dxmath(fami,epais,df,dm,dmf,pgl,multic,indith,&
     &t2ev,t2ve,t1ve,npg)
            character(len=4) :: fami
            real(kind=8) :: epais
            real(kind=8) :: df(3,3)
            real(kind=8) :: dm(3,3)
            real(kind=8) :: dmf(3,3)
            real(kind=8) :: pgl(3,3)
            integer :: multic
            integer :: indith
            real(kind=8) :: t2ev(4)
            real(kind=8) :: t2ve(4)
            real(kind=8) :: t1ve(9)
            integer :: npg
          end subroutine dxmath
        end interface
