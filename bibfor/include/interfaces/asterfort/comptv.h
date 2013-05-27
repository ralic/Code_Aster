        interface
          subroutine comptv(nbpt,fn,offset,t,nbchoc,tchmin,tchmax,&
     &tchoct,tchocm,nbrebo,trebot,trebom)
            integer :: nbpt
            real(kind=8) :: fn(*)
            real(kind=8) :: offset
            real(kind=8) :: t(*)
            integer :: nbchoc
            real(kind=8) :: tchmin
            real(kind=8) :: tchmax
            real(kind=8) :: tchoct
            real(kind=8) :: tchocm
            integer :: nbrebo
            real(kind=8) :: trebot
            real(kind=8) :: trebom
          end subroutine comptv
        end interface
