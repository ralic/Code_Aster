        interface
          subroutine dpvpdi(nbmat,mater,td,tf,tr,depst,deps)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: td
            real(kind=8) :: tf
            real(kind=8) :: tr
            real(kind=8) :: depst(6)
            real(kind=8) :: deps(6)
          end subroutine dpvpdi
        end interface
