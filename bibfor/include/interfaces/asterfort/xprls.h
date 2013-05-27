        interface
          subroutine xprls(noma,cnsln,cnslt,grln,grlt,cnsvn,cnsvt,&
     &cnsbl,deltat,nodtor,eletor,liggrd,delta)
            character(len=8) :: noma
            character(len=19) :: cnsln
            character(len=19) :: cnslt
            character(len=19) :: grln
            character(len=19) :: grlt
            character(len=19) :: cnsvn
            character(len=19) :: cnsvt
            character(len=19) :: cnsbl
            real(kind=8) :: deltat
            character(len=19) :: nodtor
            character(len=19) :: eletor
            character(len=19) :: liggrd
            character(len=19) :: delta
          end subroutine xprls
        end interface
