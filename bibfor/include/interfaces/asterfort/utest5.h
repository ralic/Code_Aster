        interface
          subroutine utest5(cham19,nomail,nocmp,tbtxt,refi,refr,refc,&
     &typres,epsi,crit,ific,llab)
            character(*) :: cham19
            character(*) :: nomail
            character(*) :: nocmp
            character(len=16) :: tbtxt(2)
            integer :: refi
            real(kind=8) :: refr
            complex(kind=8) :: refc
            character(*) :: typres
            real(kind=8) :: epsi
            character(*) :: crit
            integer :: ific
            logical :: llab
          end subroutine utest5
        end interface
