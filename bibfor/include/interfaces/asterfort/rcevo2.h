        interface
          subroutine rcevo2(nbinti,kinti,csigm,cinst,csiex,kemixt,&
     &cstex,csmex,lfatig,flexio,lrocht,cnoc,cresu,cpres)
            integer :: nbinti
            character(len=16) :: kinti
            character(len=24) :: csigm
            character(len=24) :: cinst
            character(len=24) :: csiex
            logical :: kemixt
            character(len=24) :: cstex
            character(len=24) :: csmex
            logical :: lfatig
            logical :: flexio
            logical :: lrocht
            character(len=24) :: cnoc
            character(len=24) :: cresu
            character(len=24) :: cpres
          end subroutine rcevo2
        end interface
