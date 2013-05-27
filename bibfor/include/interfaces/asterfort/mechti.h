        interface
          subroutine mechti(noma,inst,deltat,theta,chtime)
            character(*) :: noma
            real(kind=8) :: inst
            real(kind=8) :: deltat
            real(kind=8) :: theta
            character(len=24) :: chtime
          end subroutine mechti
        end interface
