        interface
          subroutine btldth(fami,xi3,nb1,kpg,btild,wgt,indic,young,nu,&
     &alpha,temper,forthi)
            character(len=4) :: fami
            real(kind=8) :: xi3
            integer :: nb1
            integer :: kpg
            real(kind=8) :: btild(5,42)
            real(kind=8) :: wgt
            integer :: indic
            real(kind=8) :: young
            real(kind=8) :: nu
            real(kind=8) :: alpha
            real(kind=8) :: temper
            real(kind=8) :: forthi(1)
          end subroutine btldth
        end interface
