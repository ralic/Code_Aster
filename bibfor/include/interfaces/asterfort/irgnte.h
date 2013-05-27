        interface
          subroutine irgnte(ifi,nbordr,coord,connex,point,njvmai,nbmai&
     &,cnsv,partie,jtype,cnsd)
            integer :: ifi
            integer :: nbordr
            real(kind=8) :: coord(*)
            integer :: connex(*)
            integer :: point(*)
            character(*) :: njvmai
            integer :: nbmai
            integer :: cnsv(*)
            character(*) :: partie
            integer :: jtype
            integer :: cnsd(*)
          end subroutine irgnte
        end interface
