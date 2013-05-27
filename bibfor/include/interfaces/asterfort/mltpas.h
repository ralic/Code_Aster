        interface
          subroutine mltpas(nbnd,nbsn,supnd,xadj,adjncy,anc,nouv,seq,&
     &global,adress,nblign,lgsn,nbloc,ncbloc,lgbloc,diag,col,lmat,place)
            integer :: nbsn
            integer :: nbnd
            integer :: supnd(nbsn+1)
            integer :: xadj(nbnd+1)
            integer :: adjncy(*)
            integer :: anc(nbnd)
            integer :: nouv(nbnd)
            integer :: seq(nbsn)
            integer(kind=4) :: global(*)
            integer :: adress(nbsn+1)
            integer :: nblign(nbsn)
            integer :: lgsn(nbsn)
            integer :: nbloc
            integer :: ncbloc(*)
            integer :: lgbloc(*)
            integer :: diag(0:nbnd)
            integer :: col(*)
            integer :: lmat
            integer :: place(nbnd)
          end subroutine mltpas
        end interface
