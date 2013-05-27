        interface
          subroutine amumpu(option,type,kxmps,usersm,nprec,lresol,&
     &kvers)
            integer :: option
            character(len=1) :: type
            integer :: kxmps
            character(len=12) :: usersm
            integer :: nprec
            logical :: lresol
            character(len=24) :: kvers
          end subroutine amumpu
        end interface
