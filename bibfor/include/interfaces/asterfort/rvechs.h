        interface
          subroutine rvechs(ssch19,nbcp,nbco,nbsp,ma,vlc,for,fex,rsor,&
     &rsex,n,ptadr,val,nbndf,clocf)
            character(len=19) :: ssch19
            integer :: nbcp
            integer :: nbco
            integer :: nbsp
            integer :: ma(*)
            integer :: vlc(*)
            integer :: for(*)
            integer :: fex(*)
            real(kind=8) :: rsor(*)
            real(kind=8) :: rsex(*)
            integer :: n
            integer :: ptadr
            real(kind=8) :: val(*)
            integer :: nbndf(6,*)
            integer :: clocf(6,4,*)
          end subroutine rvechs
        end interface
