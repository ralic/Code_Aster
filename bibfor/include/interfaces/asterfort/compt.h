        interface
          subroutine compt(nbpt,fn,offset,t,elapse,nbchoc,tchocm,&
     &tchmax,tchmin,nbrebo,trebom,tchoct,nbinst)
            integer :: nbpt
            real(kind=8) :: fn(*)
            real(kind=8) :: offset
            real(kind=8) :: t(*)
            real(kind=8) :: elapse
            integer :: nbchoc
            real(kind=8) :: tchocm
            real(kind=8) :: tchmax
            real(kind=8) :: tchmin
            integer :: nbrebo
            real(kind=8) :: trebom
            real(kind=8) :: tchoct
            integer :: nbinst
          end subroutine compt
        end interface
