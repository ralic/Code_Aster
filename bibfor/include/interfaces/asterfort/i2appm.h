        interface
          subroutine i2appm(xp,yp,xso,yso,xin,yin,cdroi,nbc,dedans)
            real(kind=8) :: xp
            real(kind=8) :: yp
            real(kind=8) :: xso(*)
            real(kind=8) :: yso(*)
            real(kind=8) :: xin(*)
            real(kind=8) :: yin(*)
            logical :: cdroi(*)
            integer :: nbc
            logical :: dedans
          end subroutine i2appm
        end interface
