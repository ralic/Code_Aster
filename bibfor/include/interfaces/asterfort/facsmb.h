        interface
          subroutine facsmb(nbnd,nbsn,supnd,invsup,parent,xadj,adjncy,&
     &anc,nouv,fils,frere,local,global,adress,lfront,nblign,lgsn,debfac,&
     &debfsn,chaine,place,nbass,delg,lgind,ier)
            integer :: lgind
            integer :: nbsn
            integer :: nbnd
            integer :: supnd(nbsn+1)
            integer :: invsup(nbnd)
            integer :: parent(nbsn)
            integer :: xadj(nbnd+1)
            integer :: adjncy(*)
            integer :: anc(nbnd)
            integer :: nouv(nbnd)
            integer :: fils(nbsn)
            integer :: frere(nbsn)
            integer(kind=4) :: local(lgind)
            integer(kind=4) :: global(lgind)
            integer :: adress(nbsn+1)
            integer :: lfront(nbsn)
            integer :: nblign(nbsn)
            integer :: lgsn(nbsn)
            integer :: debfac(nbnd+1)
            integer :: debfsn(nbsn+1)
            integer :: chaine(nbnd)
            integer :: place(nbnd)
            integer :: nbass(nbsn)
            integer :: delg(nbnd)
            integer :: ier
          end subroutine facsmb
        end interface
