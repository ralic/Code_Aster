        interface
          subroutine t3gsie(option,fami,xyzl,pgl,depl,nbcou,cdl)
            character(len=16) :: option
            character(len=4) :: fami
            real(kind=8) :: xyzl(3,*)
            real(kind=8) :: pgl(3,*)
            real(kind=8) :: depl(*)
            integer :: nbcou
            real(kind=8) :: cdl(*)
          end subroutine t3gsie
        end interface
