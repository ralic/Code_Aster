        interface
          subroutine rvgnoe(mcf,iocc,nmaila,nlstnd,nbtrou,linoeu)
            character(*) :: mcf
            integer :: iocc
            character(len=8) :: nmaila
            character(len=24) :: nlstnd
            integer :: nbtrou
            integer :: linoeu(*)
          end subroutine rvgnoe
        end interface
