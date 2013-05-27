        interface
          subroutine nvithm(compor,meca,thmc,ther,hydr,nvim,nvit,nvih,&
     &nvic,advime,advith,advihy,advico,vihrho,vicphi,vicpvp,vicsat,&
     &vicpr1,vicpr2)
            character(len=16) :: compor(*)
            character(len=16) :: meca
            character(len=16) :: thmc
            character(len=16) :: ther
            character(len=16) :: hydr
            integer :: nvim
            integer :: nvit
            integer :: nvih
            integer :: nvic
            integer :: advime
            integer :: advith
            integer :: advihy
            integer :: advico
            integer :: vihrho
            integer :: vicphi
            integer :: vicpvp
            integer :: vicsat
            integer :: vicpr1
            integer :: vicpr2
          end subroutine nvithm
        end interface
