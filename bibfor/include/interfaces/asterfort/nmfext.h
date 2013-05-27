        interface
          subroutine nmfext(eta,fonact,sddyna,veasse,cnfext)
            real(kind=8) :: eta
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=19) :: veasse(*)
            character(len=19) :: cnfext
          end subroutine nmfext
        end interface
