        interface
          subroutine xprreo(noma,fiss,noesom,noresi,cnsln,cnslt,cnsgln&
     &,cnsglt,deltat,isozro,cnxinv,nodtor,eletor,liggrd)
            character(len=8) :: noma
            character(len=8) :: fiss
            character(len=19) :: noesom
            character(len=19) :: noresi
            character(len=19) :: cnsln
            character(len=19) :: cnslt
            character(len=19) :: cnsgln
            character(len=19) :: cnsglt
            real(kind=8) :: deltat
            character(len=19) :: isozro
            character(len=19) :: cnxinv
            character(len=19) :: nodtor
            character(len=19) :: eletor
            character(len=19) :: liggrd
          end subroutine xprreo
        end interface
