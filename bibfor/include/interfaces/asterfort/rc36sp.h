        interface
          subroutine rc36sp(nbm,ima,ipt,c,k,cara,mati,pi,mi,matj,pj,mj&
     &,mse,nbthp,nbthq,ioc1,ioc2,spij,typeke,spmeca,spther)
            integer :: nbm
            integer :: ima(*)
            integer :: ipt
            real(kind=8) :: c(*)
            real(kind=8) :: k(*)
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
            real(kind=8) :: spij
            real(kind=8) :: typeke
            real(kind=8) :: spmeca
            real(kind=8) :: spther
          end subroutine rc36sp
        end interface
