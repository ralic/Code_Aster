        interface
          subroutine irgmce(chamsy,partie,ifi,nomcon,ordr,nbordr,coord&
     &,connx,point,nobj,nbel,nbcmpi,nomcmp,lresu,para,nomaou,nomain,&
     &versio,tycha)
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
            character(len=8) :: nomain
            integer :: versio
            character(len=8) :: tycha
          end subroutine irgmce
        end interface
