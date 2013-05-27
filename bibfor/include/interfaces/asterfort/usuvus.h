        interface
          subroutine usuvus(puusur,vusur,nbinst,temps,isupp,nbpt,fn,vg&
     &,iret)
            real(kind=8) :: puusur
            real(kind=8) :: vusur(*)
            integer :: nbinst
            real(kind=8) :: temps(*)
            integer :: isupp
            integer :: nbpt
            real(kind=8) :: fn(*)
            real(kind=8) :: vg(*)
            integer :: iret
          end subroutine usuvus
        end interface
