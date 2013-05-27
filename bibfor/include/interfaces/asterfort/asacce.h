        interface
          subroutine asacce(nomsy,monoap,muapde,nbsup,neq,nbmode,id,&
     &nume,vecmod,parmod,spectr,recmor,recmod,nbdis)
            integer :: nbmode
            integer :: neq
            integer :: nbsup
            character(len=16) :: nomsy
            logical :: monoap
            logical :: muapde
            integer :: id
            character(*) :: nume
            real(kind=8) :: vecmod(neq,*)
            real(kind=8) :: parmod(nbmode,*)
            real(kind=8) :: spectr(*)
            real(kind=8) :: recmor(nbsup,neq,*)
            real(kind=8) :: recmod(nbsup,neq,*)
            integer :: nbdis(nbsup)
          end subroutine asacce
        end interface
