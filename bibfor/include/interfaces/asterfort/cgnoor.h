        interface
          subroutine cgnoor(mafour,nomail,motfac,iocc,nbmc,motcle,&
     &typmcl,typlig,nbma,ndorig,ndextr,typm,vecori)
            character(len=24) :: mafour
            character(len=8) :: nomail
            character(*) :: motfac
            integer :: iocc
            integer :: nbmc
            character(len=16) :: motcle(*)
            character(len=16) :: typmcl(*)
            character(*) :: typlig
            integer :: nbma
            character(len=8) :: ndorig
            character(len=8) :: ndextr
            character(len=8) :: typm
            real(kind=8) :: vecori(3)
          end subroutine cgnoor
        end interface
