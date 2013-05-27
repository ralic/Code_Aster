        interface
          subroutine lobs(sdobse,numins,inst,lobsv)
            character(len=19) :: sdobse
            integer :: numins
            real(kind=8) :: inst
            logical :: lobsv
          end subroutine lobs
        end interface
