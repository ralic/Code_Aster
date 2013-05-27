        interface
          subroutine xprupw(cmnd,noma,fispre,vcn,grlr,noesom,lcmin,&
     &cnsln,grln,cnslt,grlt,deltat,noresi,isozro,nodtor,eletor,liggrd)
            character(len=8) :: cmnd
            character(len=8) :: noma
            character(len=8) :: fispre
            character(len=24) :: vcn
            character(len=24) :: grlr
            character(len=19) :: noesom
            real(kind=8) :: lcmin
            character(len=19) :: cnsln
            character(len=19) :: grln
            character(len=19) :: cnslt
            character(len=19) :: grlt
            real(kind=8) :: deltat
            character(len=19) :: noresi
            character(len=19) :: isozro
            character(len=19) :: nodtor
            character(len=19) :: eletor
            character(len=19) :: liggrd
          end subroutine xprupw
        end interface
