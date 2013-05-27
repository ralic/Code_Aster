        interface
          subroutine hypcpc(c11,c22,c33,c12,k,c10,c01,c20,nitmax,epsi,&
     &sig,codret)
            real(kind=8) :: c11
            real(kind=8) :: c22
            real(kind=8) :: c33
            real(kind=8) :: c12
            real(kind=8) :: k
            real(kind=8) :: c10
            real(kind=8) :: c01
            real(kind=8) :: c20
            integer :: nitmax
            real(kind=8) :: epsi
            real(kind=8) :: sig(6)
            integer :: codret
          end subroutine hypcpc
        end interface
