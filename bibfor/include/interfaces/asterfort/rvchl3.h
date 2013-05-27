        interface
          subroutine rvchl3(vale,padr,pnsp,pnbn,ma,nbma,itypm,nbco,&
     &nbsp,nbpt,nbcp,face,cref,nbndf,clocf,conec,vlccnc,val,ptadr,tabaux&
     &)
            real(kind=8) :: vale(*)
            integer :: padr(*)
            integer :: pnsp(*)
            integer :: pnbn(*)
            integer :: ma(*)
            integer :: nbma
            integer :: itypm
            integer :: nbco
            integer :: nbsp
            integer :: nbpt
            integer :: nbcp
            integer :: face(*)
            real(kind=8) :: cref(2,*)
            integer :: nbndf(6,*)
            integer :: clocf(6,4,*)
            integer :: conec(*)
            integer :: vlccnc(*)
            real(kind=8) :: val(*)
            integer :: ptadr
            real(kind=8) :: tabaux(4,*)
          end subroutine rvchl3
        end interface
