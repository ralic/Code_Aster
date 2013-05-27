        interface
          subroutine nmequi(eta,fonact,sddyna,foiner,veasse,cnfext,&
     &cnfint)
            real(kind=8) :: eta
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=19) :: foiner
            character(len=19) :: veasse(*)
            character(len=19) :: cnfext
            character(len=19) :: cnfint
          end subroutine nmequi
        end interface
