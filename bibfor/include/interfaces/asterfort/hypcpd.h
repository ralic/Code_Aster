        interface
          subroutine hypcpd(c11,c22,c33,c12,k,c10,c01,c20,dsidep,&
     &codret)
            real(kind=8) :: c11
            real(kind=8) :: c22
            real(kind=8) :: c33
            real(kind=8) :: c12
            real(kind=8) :: k
            real(kind=8) :: c10
            real(kind=8) :: c01
            real(kind=8) :: c20
            real(kind=8) :: dsidep(6,6)
            integer :: codret
          end subroutine hypcpd
        end interface
