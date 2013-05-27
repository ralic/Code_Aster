        interface
          subroutine rvchn3(vale,padr,ma,itypm,nbpt,nbcp,face,cref,&
     &nbndf,clocf,conec,vlccnc,val,ptadr,tabaux)
            real(kind=8) :: vale(*)
            integer :: padr(*)
            integer :: ma(*)
            integer :: itypm
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
          end subroutine rvchn3
        end interface
