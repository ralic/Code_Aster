        interface
          subroutine mdfedy(nbpal,nbmode,numpas,dt,dtsto,tcf,vrotat,&
     &dplmod,depgen,vitgen,fexgen,typal,finpal,cnpal,prdeff,conv,fsauv)
            integer :: nbmode
            integer :: nbpal
            integer :: numpas
            real(kind=8) :: dt
            real(kind=8) :: dtsto
            real(kind=8) :: tcf
            real(kind=8) :: vrotat
            real(kind=8) :: dplmod(nbpal,nbmode,*)
            real(kind=8) :: depgen(*)
            real(kind=8) :: vitgen(*)
            real(kind=8) :: fexgen(*)
            character(len=6) :: typal(20)
            character(len=3) :: finpal(20)
            character(len=8) :: cnpal(20)
            logical :: prdeff
            real(kind=8) :: conv
            real(kind=8) :: fsauv(20,3)
          end subroutine mdfedy
        end interface
