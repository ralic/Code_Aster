        interface
          subroutine irdesc(ifi,nbno,prno,nueq,nec,dg,ncmpmx,vale,&
     &nomcmp,titr,nomnoe,nomsd,nomsym,ir,numnoe,lmasu)
            integer :: ifi
            integer :: nbno
            integer :: prno(*)
            integer :: nueq(*)
            integer :: nec
            integer :: dg(*)
            integer :: ncmpmx
            complex(kind=8) :: vale(*)
            character(*) :: nomcmp(*)
            character(*) :: titr
            character(*) :: nomnoe(*)
            character(*) :: nomsd
            character(*) :: nomsym
            integer :: ir
            integer :: numnoe(*)
            logical :: lmasu
          end subroutine irdesc
        end interface
