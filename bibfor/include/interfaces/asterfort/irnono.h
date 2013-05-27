        interface
          subroutine irnono(noma,nbnoe,nbno,nonoe,nbgr,nogrn,numno,&
     &nbnot,indno,noltop)
            integer :: nbnoe
            character(*) :: noma
            integer :: nbno
            character(*) :: nonoe(*)
            integer :: nbgr
            character(*) :: nogrn(*)
            character(*) :: numno
            integer :: nbnot
            integer :: indno(nbnoe)
            character(*) :: noltop
          end subroutine irnono
        end interface
