        interface
          subroutine irgmm3(nomain,nomaou,nbmat,nummai,basz,nobj,nbel,&
     &versio)
            character(len=8) :: nomain
            character(len=8) :: nomaou
            integer :: nbmat
            integer :: nummai(*)
            character(*) :: basz
            character(len=24) :: nobj(28)
            integer :: nbel(28)
            integer :: versio
          end subroutine irgmm3
        end interface
