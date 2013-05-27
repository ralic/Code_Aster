        interface
          subroutine irdrsr(ifi,nbno,desc,nec,dg,ncmpmx,vale,nomcmp,&
     &titr,nomnoe,nomsd,nomsym,ir,numnoe,lmasu,nbcmp,ncmps,nocmpl)
            integer :: ifi
            integer :: nbno
            integer :: desc(*)
            integer :: nec
            integer :: dg(*)
            integer :: ncmpmx
            real(kind=8) :: vale(*)
            character(*) :: nomcmp(*)
            character(*) :: titr
            character(*) :: nomnoe(*)
            character(*) :: nomsd
            character(*) :: nomsym
            integer :: ir
            integer :: numnoe(*)
            logical :: lmasu
            integer :: nbcmp
            integer :: ncmps(*)
            character(*) :: nocmpl(*)
          end subroutine irdrsr
        end interface
