        interface
          subroutine cafvsu(cont,tange,maxfa,nface,fks,dfks1,dfks2,&
     &mobfa,dmob1,dmob2,dmob1v,dmob2v,flux,dflx1,dflx2,dflx1v,dflx2v,&
     &nbvois,nvoima)
            integer :: nvoima
            integer :: nface
            integer :: maxfa
            logical :: cont
            logical :: tange
            real(kind=8) :: fks(1:nface)
            real(kind=8) :: dfks1(1:maxfa+1,nface)
            real(kind=8) :: dfks2(1:maxfa+1,nface)
            real(kind=8) :: mobfa(0:nvoima,1:nface)
            real(kind=8) :: dmob1(0:nvoima,1:nface)
            real(kind=8) :: dmob2(0:nvoima,1:nface)
            real(kind=8) :: dmob1v(0:nvoima,1:nface)
            real(kind=8) :: dmob2v(0:nvoima,1:nface)
            real(kind=8) :: flux
            real(kind=8) :: dflx1(1:nface+1)
            real(kind=8) :: dflx2(1:nface+1)
            real(kind=8) :: dflx1v(1:nface)
            real(kind=8) :: dflx2v(1:nface)
            integer :: nbvois
          end subroutine cafvsu
        end interface
