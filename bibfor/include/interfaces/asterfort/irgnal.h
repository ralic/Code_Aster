        interface
          subroutine irgnal(ifi,nbordr,coord,connex,point,nocmp,nbcmp,&
     &numel,nobj,nbel,cnsc,cnsl,cnsv,partie,jtype,cnsd)
            integer :: nbcmp
            integer :: ifi
            integer :: nbordr
            real(kind=8) :: coord(*)
            integer :: connex(*)
            integer :: point(*)
            character(len=8) :: nocmp(nbcmp)
            integer :: numel
            character(*) :: nobj
            integer :: nbel
            integer :: cnsc(*)
            integer :: cnsl(*)
            integer :: cnsv(*)
            character(*) :: partie
            integer :: jtype
            integer :: cnsd(*)
          end subroutine irgnal
        end interface
