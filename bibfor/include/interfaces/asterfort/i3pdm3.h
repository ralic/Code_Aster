        interface
          subroutine i3pdm3(epsi,k,desc,desctm,conexk,coordo,pt,dedans&
     &)
            real(kind=8) :: epsi
            integer :: k
            integer :: desc(*)
            integer :: desctm(*)
            integer :: conexk(*)
            real(kind=8) :: coordo(*)
            real(kind=8) :: pt(*)
            logical :: dedans
          end subroutine i3pdm3
        end interface
