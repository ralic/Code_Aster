        interface
          subroutine reci2d(lirela,mailla,nnoeca,noebe,nbcnx,cxma,&
     &normal,itria,xbar,iproj,excent)
            character(len=19) :: lirela
            character(len=8) :: mailla
            character(len=8) :: nnoeca
            integer :: noebe
            integer :: nbcnx
            integer :: cxma(*)
            real(kind=8) :: normal(*)
            integer :: itria
            real(kind=8) :: xbar(*)
            integer :: iproj
            real(kind=8) :: excent
          end subroutine reci2d
        end interface
