        interface
          subroutine lkgamp(val,varv,im,sm,ucrip,seuilp,vinm,nbmat,&
     &mater,de,deps,depsv,dgamv,depsp,dgamp,retcom)
            integer :: nbmat
            integer :: val
            integer :: varv
            real(kind=8) :: im
            real(kind=8) :: sm(6)
            real(kind=8) :: ucrip
            real(kind=8) :: seuilp
            real(kind=8) :: vinm(7)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: de(6,6)
            real(kind=8) :: deps(6)
            real(kind=8) :: depsv(6)
            real(kind=8) :: dgamv
            real(kind=8) :: depsp(6)
            real(kind=8) :: dgamp
            integer :: retcom
          end subroutine lkgamp
        end interface
