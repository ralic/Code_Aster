        interface
          subroutine pjxxut(dim,mocle,moa1,moa2,nbma1,lima1,nbno2,&
     &lino2,ma1,ma2,nbtmx,nbtm,nutm,elrf)
            integer :: nbtmx
            character(len=2) :: dim
            character(*) :: mocle
            character(len=8) :: moa1
            character(len=8) :: moa2
            integer :: nbma1
            integer :: lima1(*)
            integer :: nbno2
            integer :: lino2(*)
            character(len=8) :: ma1
            character(len=8) :: ma2
            integer :: nbtm
            integer :: nutm(nbtmx)
            character(len=8) :: elrf(nbtmx)
          end subroutine pjxxut
        end interface
