        interface
          subroutine i3idks(epsi,k,desc,desctm,sgt,conexk,coordo,nbpt,&
     &lstpt)
            real(kind=8) :: epsi
            integer :: k
            integer :: desc(*)
            integer :: desctm(*)
            real(kind=8) :: sgt(*)
            integer :: conexk(*)
            real(kind=8) :: coordo(*)
            integer :: nbpt
            integer :: lstpt(*)
          end subroutine i3idks
        end interface
