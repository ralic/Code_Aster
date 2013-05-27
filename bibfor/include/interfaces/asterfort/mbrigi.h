        interface
          subroutine mbrigi(fami,kpg,imate,rig)
            character(len=4) :: fami
            integer :: kpg
            integer :: imate
            real(kind=8) :: rig(3,3)
          end subroutine mbrigi
        end interface
