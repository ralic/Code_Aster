        interface
          subroutine ascorm(monoap,typcmo,nbsup,nsupp,neq,nbmode,&
     &repmo1,repmo2,amort,modal,id,temps,recmor,recmop,tabs,nomsy,vecmod&
     &,reasup,spectr,corfre,muapde,tcosup,nintra,nbdis,f1gup,f2gup)
            integer :: nbmode
            integer :: neq
            integer :: nbsup
            logical :: monoap
            character(*) :: typcmo
            integer :: nsupp(*)
            real(kind=8) :: repmo1(nbsup,neq,*)
            real(kind=8) :: repmo2(nbsup,neq,*)
            real(kind=8) :: amort(*)
            real(kind=8) :: modal(nbmode,*)
            integer :: id
            real(kind=8) :: temps
            real(kind=8) :: recmor(nbsup,neq,*)
            real(kind=8) :: recmop(nbsup,neq,*)
            real(kind=8) :: tabs(nbsup,*)
            character(len=16) :: nomsy
            real(kind=8) :: vecmod(neq,*)
            real(kind=8) :: reasup(nbsup,nbmode,*)
            real(kind=8) :: spectr(*)
            logical :: corfre
            logical :: muapde
            integer :: tcosup(nbsup,*)
            integer :: nintra
            integer :: nbdis(nbsup)
            real(kind=8) :: f1gup
            real(kind=8) :: f2gup
          end subroutine ascorm
        end interface
