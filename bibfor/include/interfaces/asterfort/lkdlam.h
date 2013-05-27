        interface
          subroutine lkdlam(varv,nbmat,mater,deps,depsv,dgamv,im,sm,&
     &vinm,de,ucrip,seuilp,gp,devgii,paraep,varpl,dfdsp,dlam)
            integer :: nbmat
            integer :: varv
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: depsv(6)
            real(kind=8) :: dgamv
            real(kind=8) :: im
            real(kind=8) :: sm(6)
            real(kind=8) :: vinm(7)
            real(kind=8) :: de(6,6)
            real(kind=8) :: ucrip
            real(kind=8) :: seuilp
            real(kind=8) :: gp(6)
            real(kind=8) :: devgii
            real(kind=8) :: paraep(3)
            real(kind=8) :: varpl(4)
            real(kind=8) :: dfdsp(6)
            real(kind=8) :: dlam
          end subroutine lkdlam
        end interface
