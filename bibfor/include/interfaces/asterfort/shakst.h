        interface
          subroutine shakst(kstab,k11,k22,k33,k12,k21,k13,k23,k31,k32)
            real(kind=8) :: kstab(24,24)
            real(kind=8) :: k11(8,8)
            real(kind=8) :: k22(8,8)
            real(kind=8) :: k33(8,8)
            real(kind=8) :: k12(8,8)
            real(kind=8) :: k21(8,8)
            real(kind=8) :: k13(8,8)
            real(kind=8) :: k23(8,8)
            real(kind=8) :: k31(8,8)
            real(kind=8) :: k32(8,8)
          end subroutine shakst
        end interface
