        interface
          subroutine lkdgde(val,vintr,dt,seuive,ucrim,im,sm,vinm,nbmat&
     &,mater,depsv,dgamv,retcom)
            integer :: nbmat
            integer :: val
            real(kind=8) :: vintr
            real(kind=8) :: dt
            real(kind=8) :: seuive
            real(kind=8) :: ucrim
            real(kind=8) :: im
            real(kind=8) :: sm(6)
            real(kind=8) :: vinm(7)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: depsv(6)
            real(kind=8) :: dgamv
            integer :: retcom
          end subroutine lkdgde
        end interface
