        interface
          subroutine dpvpdb(nbmat,mater,crit,dt,vinm,vinp,nvi,seqe,i1e&
     &,seqm,i1m,dp,nbre,retcom)
            integer :: nvi
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: crit(3)
            real(kind=8) :: dt
            real(kind=8) :: vinm(nvi)
            real(kind=8) :: vinp(nvi)
            real(kind=8) :: seqe
            real(kind=8) :: i1e
            real(kind=8) :: seqm
            real(kind=8) :: i1m
            real(kind=8) :: dp
            integer :: nbre
            integer :: retcom
          end subroutine dpvpdb
        end interface
