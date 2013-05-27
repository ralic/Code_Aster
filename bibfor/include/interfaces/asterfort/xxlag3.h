        interface
          subroutine xxlag3(ffc,idepl,idepm,idep0,lact,ndim,nnol,pla,&
     &lamb,nvec)
            real(kind=8) :: ffc(8)
            integer :: idepl
            integer :: idepm
            integer :: idep0
            integer :: lact(8)
            integer :: ndim
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: lamb(3)
            integer :: nvec
          end subroutine xxlag3
        end interface
