        interface
          subroutine xmmjeu(ndim,jnnm,jnne,ndeple,nsinge,nsingm,ffe,&
     &ffm,norm,jgeom,jdepde,jdepm,rre,rrm,jddle,jddlm,nfhe,nfhm,lmulti,&
     &heavfa,jeu)
            integer :: ndim
            integer :: jnnm(3)
            integer :: jnne(3)
            integer :: ndeple
            integer :: nsinge
            integer :: nsingm
            real(kind=8) :: ffe(20)
            real(kind=8) :: ffm(20)
            real(kind=8) :: norm(3)
            integer :: jgeom
            integer :: jdepde
            integer :: jdepm
            real(kind=8) :: rre
            real(kind=8) :: rrm
            integer :: jddle(2)
            integer :: jddlm(2)
            integer :: nfhe
            integer :: nfhm
            logical :: lmulti
            integer :: heavfa(*)
            real(kind=8) :: jeu
          end subroutine xmmjeu
        end interface
