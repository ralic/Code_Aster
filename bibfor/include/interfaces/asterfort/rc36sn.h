        interface
          subroutine rc36sn(nbm,adrm,ipt,c,cara,mati,pi,mi,matj,pj,mj,&
     &mse,nbthp,nbthq,ioc1,ioc2,snij)
            integer :: nbm
            integer :: adrm(*)
            integer :: ipt
            real(kind=8) :: c(*)
            real(kind=8) :: cara(*)
            real(kind=8) :: mati(*)
            real(kind=8) :: pi
            real(kind=8) :: mi(*)
            real(kind=8) :: matj(*)
            real(kind=8) :: pj
            real(kind=8) :: mj(*)
            real(kind=8) :: mse(*)
            integer :: nbthp
            integer :: nbthq
            integer :: ioc1
            integer :: ioc2
            real(kind=8) :: snij
          end subroutine rc36sn
        end interface
