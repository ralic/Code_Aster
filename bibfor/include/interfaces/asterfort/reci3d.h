        interface
          subroutine reci3d(lirela,mailla,nnoeca,noebe,nbcnx,cxma,&
     &itetra,xbar,immer)
            character(len=19) :: lirela
            character(len=8) :: mailla
            character(len=8) :: nnoeca
            integer :: noebe
            integer :: nbcnx
            integer :: cxma(*)
            integer :: itetra
            real(kind=8) :: xbar(*)
            integer :: immer
          end subroutine reci3d
        end interface
