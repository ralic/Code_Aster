        interface
          subroutine envdep(numpas,nbpal,dt,dtsto,temps,dep,vit,vrotat&
     &,finpal,prdeff)
            integer :: nbpal
            integer :: numpas
            real(kind=8) :: dt
            real(kind=8) :: dtsto
            real(kind=8) :: temps
            real(kind=8) :: dep(nbpal,*)
            real(kind=8) :: vit(nbpal,*)
            real(kind=8) :: vrotat
            character(len=3) :: finpal(20)
            logical :: prdeff
          end subroutine envdep
        end interface
