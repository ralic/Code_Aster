        interface
          subroutine asexci(masse,parmod,amort,nbmode,corfre,impr,ndir&
     &,monoap,muapde,kspect,kasysp,nbsup,nsupp,knoeu)
            integer :: nbmode
            character(*) :: masse
            real(kind=8) :: parmod(nbmode,*)
            real(kind=8) :: amort(*)
            logical :: corfre
            integer :: impr
            integer :: ndir(*)
            logical :: monoap
            logical :: muapde
            character(*) :: kspect
            character(*) :: kasysp
            integer :: nbsup
            integer :: nsupp(*)
            character(*) :: knoeu
          end subroutine asexci
        end interface
