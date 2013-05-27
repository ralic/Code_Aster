        interface
          subroutine xthddl(nfh,nddlno,nno,stano,option,nomte,mat,vect&
     &)
            integer :: nfh
            integer :: nddlno
            integer :: nno
            integer :: stano(*)
            character(len=16) :: option
            character(len=16) :: nomte
            real(kind=8) :: mat(*)
            real(kind=8) :: vect(*)
          end subroutine xthddl
        end interface
