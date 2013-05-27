        interface
          subroutine rcev22(nbinti,kinti,iocc,csigm,cinst,ccont,lfatig&
     &,flexio,lrocht,cnoc,cresu,cpres)
            integer :: nbinti
            character(len=16) :: kinti
            integer :: iocc
            character(len=24) :: csigm
            character(len=24) :: cinst
            character(len=24) :: ccont
            logical :: lfatig
            logical :: flexio
            logical :: lrocht
            character(len=24) :: cnoc
            character(len=24) :: cresu
            character(len=24) :: cpres
          end subroutine rcev22
        end interface
