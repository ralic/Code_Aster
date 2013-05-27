        interface
          subroutine pochpv(trange,nbbloc,tdebut,tfin,offset,trepos,&
     &nbclas,nomres,loptio)
            character(*) :: trange
            integer :: nbbloc
            real(kind=8) :: tdebut
            real(kind=8) :: tfin
            real(kind=8) :: offset
            real(kind=8) :: trepos
            integer :: nbclas
            character(*) :: nomres
            logical :: loptio
          end subroutine pochpv
        end interface
