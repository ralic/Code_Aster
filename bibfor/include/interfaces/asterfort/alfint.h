        interface
          subroutine alfint(chmatz,imate,nommaz,tdef,noparz,nummat,&
     &prec,ch19)
            character(*) :: chmatz
            integer :: imate
            character(*) :: nommaz
            real(kind=8) :: tdef
            character(*) :: noparz
            integer :: nummat
            real(kind=8) :: prec
            character(len=19) :: ch19
          end subroutine alfint
        end interface
