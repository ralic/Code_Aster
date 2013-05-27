        interface
          subroutine bmradi(basmod,intf,nomint,numint,nbddl,ivddl,&
     &nbdif)
            integer :: nbddl
            character(len=8) :: basmod
            character(len=8) :: intf
            character(len=8) :: nomint
            integer :: numint
            integer :: ivddl(nbddl)
            integer :: nbdif
          end subroutine bmradi
        end interface
