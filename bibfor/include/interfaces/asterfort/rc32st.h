        interface
          subroutine rc32st(sijm,nbinst,sth,sn)
            integer :: nbinst
            real(kind=8) :: sijm(6)
            real(kind=8) :: sth(6*nbinst)
            real(kind=8) :: sn
          end subroutine rc32st
        end interface
