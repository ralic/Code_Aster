        interface
          subroutine ascarm(nomsy,monoap,nbsup,nsupp,neq,nbmode,vecmod&
     &,parmod,id,reasup,spectr,repmod,corfre,amort,muapde,tcosup,im,&
     &nbdis)
            integer :: nbmode
            integer :: neq
            integer :: nbsup
            character(len=16) :: nomsy
            logical :: monoap
            integer :: nsupp(*)
            real(kind=8) :: vecmod(neq,*)
            real(kind=8) :: parmod(nbmode,*)
            integer :: id
            real(kind=8) :: reasup(nbsup,nbmode,*)
            real(kind=8) :: spectr(*)
            real(kind=8) :: repmod(nbsup,neq,*)
            logical :: corfre
            real(kind=8) :: amort(*)
            logical :: muapde
            integer :: tcosup(nbsup,*)
            integer :: im
            integer :: nbdis(*)
          end subroutine ascarm
        end interface
