        interface
          subroutine rc36fs(nbsig1,noc1,sit1,nbsig2,noc2,sit2,saltij,&
     &ns,nscy,matse,mse,sn,nommat,c,k,cara,ug)
            integer :: nbsig1
            integer :: noc1(*)
            integer :: sit1(*)
            integer :: nbsig2
            integer :: noc2(*)
            integer :: sit2(*)
            real(kind=8) :: saltij(*)
            integer :: ns
            integer :: nscy
            real(kind=8) :: matse(*)
            real(kind=8) :: mse(*)
            real(kind=8) :: sn(*)
            character(len=8) :: nommat
            real(kind=8) :: c(*)
            real(kind=8) :: k(*)
            real(kind=8) :: cara(*)
            real(kind=8) :: ug
          end subroutine rc36fs
        end interface
