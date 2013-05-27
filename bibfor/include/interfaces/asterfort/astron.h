        interface
          subroutine astron(nomsy,psmo,monoap,muapde,nbsup,nsupp,neq,&
     &nbmode,id,vecmod,parmod,spectr,nomsup,reasup,recmor,recmop)
            integer :: nbmode
            integer :: neq
            integer :: nbsup
            character(len=16) :: nomsy
            character(*) :: psmo
            logical :: monoap
            logical :: muapde
            integer :: nsupp(*)
            integer :: id
            real(kind=8) :: vecmod(neq,*)
            real(kind=8) :: parmod(nbmode,*)
            real(kind=8) :: spectr(*)
            character(*) :: nomsup(nbsup,*)
            real(kind=8) :: reasup(nbsup,nbmode,*)
            real(kind=8) :: recmor(nbsup,neq,*)
            real(kind=8) :: recmop(nbsup,neq,*)
          end subroutine astron
        end interface
