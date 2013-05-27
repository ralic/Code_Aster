        interface
          subroutine mltdrb(nbloc,ncbloc,decal,seq,nbsn,nbnd,supnd,&
     &adress,global,lgsn,factol,factou,x,temp,invp,perm,ad,trav,typsym,&
     &nbsm,s)
            integer :: nbsm
            integer :: nbnd
            integer :: nbsn
            integer :: nbloc
            integer :: ncbloc(nbnd)
            integer :: decal(nbsn)
            integer :: seq(nbsn)
            integer :: supnd(nbsn+1)
            integer :: adress(nbsn+1)
            integer(kind=4) :: global(*)
            integer :: lgsn(nbsn)
            character(len=24) :: factol
            character(len=24) :: factou
            real(kind=8) :: x(nbnd,nbsm)
            real(kind=8) :: temp(nbnd)
            integer :: invp(nbnd)
            integer :: perm(nbnd)
            integer :: ad(nbnd)
            real(kind=8) :: trav(nbnd,nbsm)
            integer :: typsym
            real(kind=8) :: s(nbsm)
          end subroutine mltdrb
        end interface
