        interface
          subroutine pjeflo(elrefa,ndim,ipb,xr2,alarm,ma2,ino2,ma1,&
     &ima1,lext)
            integer :: ndim
            character(*) :: elrefa
            integer :: ipb
            real(kind=8) :: xr2(ndim)
            character(*) :: alarm
            character(len=8) :: ma2
            integer :: ino2
            character(len=8) :: ma1
            integer :: ima1
            logical :: lext
          end subroutine pjeflo
        end interface
