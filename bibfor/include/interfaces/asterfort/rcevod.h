        interface
          subroutine rcevod(csigm,cinst,cnoc,sm,lfatig,lpmpb,lsn,csno,&
     &csne,flexio,csneo,csnee,cfao,cfae,cspo,cspe,cresu,kinti,it,jt,&
     &lrocht,symax,cpres,kemixt,cspto,cspte,cspmo,cspme)
            character(len=24) :: csigm
            character(len=24) :: cinst
            character(len=24) :: cnoc
            real(kind=8) :: sm
            logical :: lfatig
            logical :: lpmpb
            logical :: lsn
            character(len=24) :: csno
            character(len=24) :: csne
            logical :: flexio
            character(len=24) :: csneo
            character(len=24) :: csnee
            character(len=24) :: cfao
            character(len=24) :: cfae
            character(len=24) :: cspo
            character(len=24) :: cspe
            character(len=24) :: cresu
            character(len=16) :: kinti
            integer :: it
            integer :: jt
            logical :: lrocht
            real(kind=8) :: symax
            character(len=24) :: cpres
            logical :: kemixt
            character(len=24) :: cspto
            character(len=24) :: cspte
            character(len=24) :: cspmo
            character(len=24) :: cspme
          end subroutine rcevod
        end interface
