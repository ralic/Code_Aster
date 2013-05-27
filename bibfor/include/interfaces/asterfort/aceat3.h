        interface
          subroutine aceat3(noma,nomu,nbtuy,nbpart,nbmap,elpar,nopar,&
     &ivr,ifm,nbzk,nozk,cozk,isens,coor,epsi,crit,nno,nmmt)
            integer :: nno
            integer :: nbzk
            integer :: nbpart
            integer :: nbtuy
            character(len=8) :: noma
            character(len=8) :: nomu
            integer :: nbmap(nbpart)
            integer :: elpar(nbpart,nbtuy)
            integer :: nopar(nbpart,nno,nbtuy)
            integer :: ivr(3)
            integer :: ifm
            integer :: nozk(nbzk)
            real(kind=8) :: cozk(3*nbzk)
            integer :: isens(nbpart)
            real(kind=8) :: coor(*)
            real(kind=8) :: epsi
            character(len=8) :: crit
            integer :: nmmt(*)
          end subroutine aceat3
        end interface
