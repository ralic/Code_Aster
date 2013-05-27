        interface
          subroutine irgmcn(chamsy,partie,ifi,nomcon,ordr,nbordr,coord&
     &,connx,point,nobj,nbel,nbcmpi,nomcmp,lresu,para,versio,tycha)
            character(*) :: chamsy
            character(*) :: partie
            integer :: ifi
            character(*) :: nomcon
            integer :: ordr(*)
            integer :: nbordr
            real(kind=8) :: coord(*)
            integer :: connx(*)
            integer :: point(*)
            character(len=24) :: nobj(*)
            integer :: nbel(*)
            integer :: nbcmpi
            character(*) :: nomcmp(*)
            logical :: lresu
            real(kind=8) :: para(*)
            integer :: versio
            character(len=8) :: tycha
          end subroutine irgmcn
        end interface
