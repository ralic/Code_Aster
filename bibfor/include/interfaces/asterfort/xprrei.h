        interface
          subroutine xprrei(noma,fiss,fispre,noesom,noresi,cnsln,cnslt&
     &,cnsgls,deltat,lcmin,levset,isozro,cnxinv,nodtor,eletor,liggrd)
            character(len=8) :: noma
            character(len=8) :: fiss
            character(len=8) :: fispre
            character(len=19) :: noesom
            character(len=19) :: noresi
            character(len=19) :: cnsln
            character(len=19) :: cnslt
            character(len=19) :: cnsgls
            real(kind=8) :: deltat
            real(kind=8) :: lcmin
            character(len=2) :: levset
            character(len=19) :: isozro
            character(len=19) :: cnxinv
            character(len=19) :: nodtor
            character(len=19) :: eletor
            character(len=19) :: liggrd
          end subroutine xprrei
        end interface
