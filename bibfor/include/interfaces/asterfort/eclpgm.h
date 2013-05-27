        interface
          subroutine eclpgm(ma2,mo,cham1,ligrel,shrink,lonmin,nch,&
     &lisch)
            integer :: nch
            character(len=8) :: ma2
            character(len=8) :: mo
            character(len=19) :: cham1
            character(len=19) :: ligrel
            real(kind=8) :: shrink
            real(kind=8) :: lonmin
            character(len=16) :: lisch(nch)
          end subroutine eclpgm
        end interface
