        interface
          subroutine ornofd(mafour,nomail,nbma,noeord,ndorig,ndextr,&
     &base,vecori)
            character(len=24) :: mafour
            character(len=8) :: nomail
            integer :: nbma
            character(len=24) :: noeord
            character(len=8) :: ndorig
            character(len=8) :: ndextr
            character(len=1) :: base
            real(kind=8) :: vecori(3)
          end subroutine ornofd
        end interface
