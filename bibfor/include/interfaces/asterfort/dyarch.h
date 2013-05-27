        interface
          subroutine dyarch(nbpas,lisins,lisarc,nbarch,ich,nbexcl,type&
     &)
            integer :: nbpas
            character(*) :: lisins
            character(*) :: lisarc
            integer :: nbarch
            integer :: ich
            integer :: nbexcl
            character(*) :: type(*)
          end subroutine dyarch
        end interface
