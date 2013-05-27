        interface
          subroutine irgmpv(ifi,lresu,nomcon,chamsy,nbordr,para,nocmp,&
     &nbel,scal,vect,tens,versio)
            integer :: ifi
            logical :: lresu
            character(*) :: nomcon
            character(*) :: chamsy
            integer :: nbordr
            real(kind=8) :: para(*)
            character(len=8) :: nocmp
            integer :: nbel(*)
            logical :: scal
            logical :: vect
            logical :: tens
            integer :: versio
          end subroutine irgmpv
        end interface
