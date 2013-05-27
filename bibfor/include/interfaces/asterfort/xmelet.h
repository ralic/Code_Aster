        interface
          subroutine xmelet(nomte,typmai,elrees,elrema,elreco,ndim,&
     &nddl,jnne,jnnm,nnc,jddle,jddlm,nconta,ndeple,nsinge,nsingm,nfhe,&
     &nfhm)
            character(len=16) :: nomte
            character(len=8) :: typmai
            character(len=8) :: elrees
            character(len=8) :: elrema
            character(len=8) :: elreco
            integer :: ndim
            integer :: nddl
            integer :: jnne(3)
            integer :: jnnm(3)
            integer :: nnc
            integer :: jddle(2)
            integer :: jddlm(2)
            integer :: nconta
            integer :: ndeple
            integer :: nsinge
            integer :: nsingm
            integer :: nfhe
            integer :: nfhm
          end subroutine xmelet
        end interface
