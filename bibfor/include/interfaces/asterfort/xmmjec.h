        interface
          subroutine xmmjec(ndim,jnnm,jnne,ndeple,nsinge,nsingm,ffe,&
     &ffm,norm,jgeom,jdepde,rre,rrm,jddle,jddlm,nfhe,nfhm,lmulti,heavfa,&
     &jeuca)
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
            real(kind=8) :: rre
            real(kind=8) :: rrm
            integer :: jddle(2)
            integer :: jddlm(2)
            integer :: nfhe
            integer :: nfhm
            logical :: lmulti
            integer :: heavfa(*)
            real(kind=8) :: jeuca
          end subroutine xmmjec
        end interface
