        interface
          subroutine asdir(monoap,muapde,id,neq,nbsup,nsupp,tcosup,&
     &recmod,repdir)
            integer :: nbsup
            integer :: neq
            logical :: monoap
            logical :: muapde
            integer :: id
            integer :: nsupp(*)
            integer :: tcosup(nbsup,*)
            real(kind=8) :: recmod(nbsup,neq,*)
            real(kind=8) :: repdir(neq,*)
          end subroutine asdir
        end interface
