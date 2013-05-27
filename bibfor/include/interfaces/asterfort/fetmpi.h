        interface
          subroutine fetmpi(optmpi,nbsd,ifm,niv,rang,nbproc,ach24,&
     &ach241,ach242,argr1)
            integer :: optmpi
            integer :: nbsd
            integer :: ifm
            integer :: niv
            integer :: rang
            integer :: nbproc
            character(len=24) :: ach24
            character(len=24) :: ach241
            character(len=24) :: ach242
            real(kind=8) :: argr1
          end subroutine fetmpi
        end interface
