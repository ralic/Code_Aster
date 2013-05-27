        interface
          subroutine deflg2(gn,lamb,logl,pes,feta,xi,me)
            real(kind=8) :: gn(3,3)
            real(kind=8) :: lamb(3)
            real(kind=8) :: logl(3)
            real(kind=8) :: pes(6,6)
            real(kind=8) :: feta(4)
            real(kind=8) :: xi(3,3)
            real(kind=8) :: me(3,3,3,3)
          end subroutine deflg2
        end interface
