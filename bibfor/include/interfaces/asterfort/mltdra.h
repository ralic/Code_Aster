        interface
          subroutine mltdra(nbloc,lgbloc,ncbloc,decal,seq,nbsn,nbnd,&
     &supnd,adress,global,lgsn,factol,factou,sm,x,invp,perm,ad,trav,&
     &typsym)
            integer :: nbnd
            integer :: nbsn
            integer :: nbloc
            integer :: lgbloc(nbsn)
            integer :: ncbloc(nbnd)
            integer :: decal(nbsn)
            integer :: seq(nbsn)
            integer :: supnd(nbsn+1)
            integer :: adress(nbsn+1)
            integer(kind=4) :: global(*)
            integer :: lgsn(nbsn)
            character(len=24) :: factol
            character(len=24) :: factou
            real(kind=8) :: sm(nbnd)
            real(kind=8) :: x(nbnd)
            integer :: invp(nbnd)
            integer :: perm(nbnd)
            integer :: ad(nbnd)
            real(kind=8) :: trav(nbnd)
            integer :: typsym
          end subroutine mltdra
        end interface
