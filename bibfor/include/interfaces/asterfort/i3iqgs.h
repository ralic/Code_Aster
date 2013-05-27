        interface
          subroutine i3iqgs(epsi,k,f,desc,desctm,conexk,coordo,sgt,&
     &nbpt,lstpt,fink)
            real(kind=8) :: epsi
            integer :: k
            integer :: f
            integer :: desc(*)
            integer :: desctm(*)
            integer :: conexk(*)
            real(kind=8) :: coordo(*)
            real(kind=8) :: sgt(*)
            integer :: nbpt
            integer :: lstpt(*)
            logical :: fink
          end subroutine i3iqgs
        end interface
