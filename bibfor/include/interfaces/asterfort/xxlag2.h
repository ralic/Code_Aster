        interface
          subroutine xxlag2(ffc,idepl,idepm,lact,ndim,nnol,pla,lamb,&
     &nvec)
            real(kind=8) :: ffc(8)
            integer :: idepl
            integer :: idepm
            integer :: lact(8)
            integer :: ndim
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: lamb(3)
            integer :: nvec
          end subroutine xxlag2
        end interface
