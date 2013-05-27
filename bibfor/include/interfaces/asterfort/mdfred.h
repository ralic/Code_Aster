        interface
          subroutine mdfred(nbmode,depgen,fexgen,nbrede,dplred,parred,&
     &fonred,saured,saredi)
            integer :: nbrede
            integer :: nbmode
            real(kind=8) :: depgen(*)
            real(kind=8) :: fexgen(*)
            real(kind=8) :: dplred(nbrede,nbmode,*)
            real(kind=8) :: parred(nbrede,*)
            character(len=8) :: fonred(nbrede,*)
            real(kind=8) :: saured(*)
            integer :: saredi(*)
          end subroutine mdfred
        end interface
