        interface
          subroutine fenexc(noma,nomnoa,long,nbn,nuno,diax,nbnfen,&
     &noefen,disfen)
            character(len=8) :: noma
            character(len=8) :: nomnoa
            real(kind=8) :: long
            integer :: nbn
            integer :: nuno(*)
            real(kind=8) :: diax(*)
            integer :: nbnfen
            integer :: noefen(*)
            real(kind=8) :: disfen(*)
          end subroutine fenexc
        end interface
