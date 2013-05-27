        interface
          subroutine irmad1(ifi,versio,nbno,prno,nueq,nec,dg,ncmpmx,&
     &itype,nstat,chamno,nomcmp,nomsym,numnoe)
            integer :: ifi
            integer :: versio
            integer :: nbno
            integer :: prno(*)
            integer :: nueq(*)
            integer :: nec
            integer :: dg(*)
            integer :: ncmpmx
            integer :: itype
            integer :: nstat
            character(*) :: chamno(*)
            character(*) :: nomcmp(*)
            character(*) :: nomsym
            integer :: numnoe(*)
          end subroutine irmad1
        end interface
