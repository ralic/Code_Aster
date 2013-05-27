        interface
          subroutine irdeca(ifi,nbno,prno,nueq,nec,dg,ncmpmx,vale,&
     &nomgd,ncmpgd,nomsym,numnoe,lresu,nbcput,ncmput,nive)
            integer :: ifi
            integer :: nbno
            integer :: prno(*)
            integer :: nueq(*)
            integer :: nec
            integer :: dg(*)
            integer :: ncmpmx
            real(kind=8) :: vale(*)
            character(*) :: nomgd
            character(*) :: ncmpgd(*)
            character(*) :: nomsym
            integer :: numnoe(*)
            logical :: lresu
            integer :: nbcput
            character(*) :: ncmput(*)
            integer :: nive
          end subroutine irdeca
        end interface
