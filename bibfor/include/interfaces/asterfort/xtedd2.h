        interface
          subroutine xtedd2(ndim,jnne,ndeple,jnnm,nddl,option,lesclx,&
     &lmaitx,lcontx,stano,lact,jddle,jddlm,nfhe,nfhm,lmulti,heavno,mmat,&
     &vtmp)
            integer :: ndim
            integer :: jnne(3)
            integer :: ndeple
            integer :: jnnm(3)
            integer :: nddl
            character(len=16) :: option
            logical :: lesclx
            logical :: lmaitx
            logical :: lcontx
            integer :: stano(*)
            integer :: lact(8)
            integer :: jddle(2)
            integer :: jddlm(2)
            integer :: nfhe
            integer :: nfhm
            logical :: lmulti
            integer :: heavno(8)
            real(kind=8) :: mmat(336,336)
            real(kind=8) :: vtmp(336)
          end subroutine xtedd2
        end interface
