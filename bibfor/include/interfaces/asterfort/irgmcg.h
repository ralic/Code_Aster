        interface
          subroutine irgmcg(chamsy,partie,ifi,nomcon,ordr,nbordr,coord&
     &,connx,point,nobj,nbel,nbcmpi,nomcmp,lresu,para,nomaou,versio)
            character(*) :: chamsy
            character(*) :: partie
            integer :: ifi
            character(*) :: nomcon
            integer :: ordr(*)
            integer :: nbordr
            real(kind=8) :: coord(*)
            integer :: connx(*)
            integer :: point(*)
            character(len=24) :: nobj(28)
            integer :: nbel(28)
            integer :: nbcmpi
            character(*) :: nomcmp(*)
            logical :: lresu
            real(kind=8) :: para(*)
            character(len=8) :: nomaou
            integer :: versio
          end subroutine irgmcg
        end interface
