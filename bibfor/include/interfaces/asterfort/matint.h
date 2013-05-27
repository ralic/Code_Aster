        interface
          subroutine matint(kr,mr,direc,vtest,rayon)
            real(kind=8) :: kr(12,12)
            real(kind=8) :: mr(12,12)
            real(kind=8) :: direc(3)
            real(kind=8) :: vtest(3)
            real(kind=8) :: rayon
          end subroutine matint
        end interface
