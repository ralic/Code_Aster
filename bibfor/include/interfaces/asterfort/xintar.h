        interface
          subroutine xintar(elp,ndim,ia,tabco,tabls,intar)
            integer :: ndim
            character(len=8) :: elp
            integer :: ia
            real(kind=8) :: tabco(*)
            real(kind=8) :: tabls(*)
            real(kind=8) :: intar(ndim)
          end subroutine xintar
        end interface
