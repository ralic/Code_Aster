        interface
          subroutine xxlagm(ffc,idepl,idepm,lact,ndim,nnol,pla,reac,&
     &reac12,tau1,tau2,nvec)
            real(kind=8) :: ffc(8)
            integer :: idepl
            integer :: idepm
            integer :: lact(8)
            integer :: ndim
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: reac
            real(kind=8) :: reac12(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            integer :: nvec
          end subroutine xxlagm
        end interface
