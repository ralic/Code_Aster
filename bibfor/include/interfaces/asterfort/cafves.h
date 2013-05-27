        interface
          subroutine cafves(cont,tange,maxfa,nface,fks,dfks1,dfks2,&
     &mobfa,dmob1,dmob2,mob1f,mob2f,flux,dflx1,dflx2)
            integer :: maxfa
            logical :: cont
            logical :: tange
            integer :: nface
            real(kind=8) :: fks(maxfa)
            real(kind=8) :: dfks1(maxfa+1,maxfa)
            real(kind=8) :: dfks2(maxfa+1,maxfa)
            real(kind=8) :: mobfa(maxfa)
            real(kind=8) :: dmob1(maxfa)
            real(kind=8) :: dmob2(maxfa)
            real(kind=8) :: mob1f(maxfa)
            real(kind=8) :: mob2f(maxfa)
            real(kind=8) :: flux
            real(kind=8) :: dflx1(maxfa+1)
            real(kind=8) :: dflx2(maxfa+1)
          end subroutine cafves
        end interface
