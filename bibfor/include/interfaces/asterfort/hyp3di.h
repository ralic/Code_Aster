        interface
          subroutine hyp3di(c11,c22,c33,c12,c13,c23,c10,c01,c20,ciso,&
     &codret)
            real(kind=8) :: c11
            real(kind=8) :: c22
            real(kind=8) :: c33
            real(kind=8) :: c12
            real(kind=8) :: c13
            real(kind=8) :: c23
            real(kind=8) :: c10
            real(kind=8) :: c01
            real(kind=8) :: c20
            real(kind=8) :: ciso(6,6)
            integer :: codret
          end subroutine hyp3di
        end interface
