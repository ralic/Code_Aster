        interface
          subroutine ntobsv(noma,sdieto,sdobse,numins,inst)
            character(len=8) :: noma
            character(len=24) :: sdieto
            character(len=19) :: sdobse
            integer :: numins
            real(kind=8) :: inst
          end subroutine ntobsv
        end interface
