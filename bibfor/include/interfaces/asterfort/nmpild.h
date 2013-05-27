        interface
          subroutine nmpild(numedd,sddyna,solalg,eta,rho,offset)
            character(len=24) :: numedd
            character(len=19) :: sddyna
            character(len=19) :: solalg(*)
            real(kind=8) :: eta
            real(kind=8) :: rho
            real(kind=8) :: offset
          end subroutine nmpild
        end interface
