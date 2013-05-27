        interface
          subroutine bmrdda(basmod,intf,nomint,numint,nbddl,ivddl,&
     &nbdif,ord,nliais)
            integer :: nbddl
            character(len=8) :: basmod
            character(len=8) :: intf
            character(len=8) :: nomint
            integer :: numint
            integer :: ivddl(nbddl)
            integer :: nbdif
            integer :: ord
            integer :: nliais
          end subroutine bmrdda
        end interface
