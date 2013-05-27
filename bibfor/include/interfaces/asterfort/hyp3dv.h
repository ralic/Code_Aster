        interface
          subroutine hyp3dv(c11,c22,c33,c12,c13,c23,k,cvol,codret)
            real(kind=8) :: c11
            real(kind=8) :: c22
            real(kind=8) :: c33
            real(kind=8) :: c12
            real(kind=8) :: c13
            real(kind=8) :: c23
            real(kind=8) :: k
            real(kind=8) :: cvol(6,6)
            integer :: codret
          end subroutine hyp3dv
        end interface
