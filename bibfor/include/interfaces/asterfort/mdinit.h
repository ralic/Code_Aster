        interface
          subroutine mdinit(basemo,nbmode,nbchoc,depgen,vitgen,vint,&
     &ier,tinit)
            character(len=8) :: basemo
            integer :: nbmode
            integer :: nbchoc
            real(kind=8) :: depgen(*)
            real(kind=8) :: vitgen(*)
            real(kind=8) :: vint(*)
            integer :: ier
            real(kind=8) :: tinit
          end subroutine mdinit
        end interface
