        interface
          subroutine dpfch3(nno,nnf,poids,dfrdef,dfrdnf,dfrdkf,coor,&
     &dfrdeg,dfrdng,dfrdkg,dfdx,dfdy,dfdz,jac)
            integer :: nno
            integer :: nnf
            real(kind=8) :: poids
            real(kind=8) :: dfrdef(1)
            real(kind=8) :: dfrdnf(1)
            real(kind=8) :: dfrdkf(1)
            real(kind=8) :: coor(1)
            real(kind=8) :: dfrdeg(1)
            real(kind=8) :: dfrdng(1)
            real(kind=8) :: dfrdkg(1)
            real(kind=8) :: dfdx(1)
            real(kind=8) :: dfdy(1)
            real(kind=8) :: dfdz(1)
            real(kind=8) :: jac
          end subroutine dpfch3
        end interface
