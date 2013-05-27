        interface
          subroutine pmsta1(sigm,sigp,deps,vim,vip,nbvari,nbvita,&
     &iforta,nbpar,nompar,vr,igrad,typpar,nomvi,sddisc,liccvg,itemax,&
     &conver,actite)
            real(kind=8) :: sigm(6)
            real(kind=8) :: sigp(6)
            real(kind=8) :: deps(9)
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
            integer :: nbvari
            integer :: nbvita
            integer :: iforta
            integer :: nbpar
            character(len=16) :: nompar(*)
            real(kind=8) :: vr(*)
            integer :: igrad
            character(len=8) :: typpar(*)
            character(len=8) :: nomvi(*)
            character(len=19) :: sddisc
            integer :: liccvg(5)
            logical :: itemax
            logical :: conver
            integer :: actite
          end subroutine pmsta1
        end interface
