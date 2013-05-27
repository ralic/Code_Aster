        interface
          subroutine utest0(nomta,para,typtes,typres,tbtxt,refi,refr,&
     &refc,epsi,crit,ific,llab,ssigne)
            character(*) :: nomta
            character(*) :: para
            character(len=8) :: typtes
            character(*) :: typres
            character(len=16) :: tbtxt(2)
            integer :: refi
            real(kind=8) :: refr
            complex(kind=8) :: refc
            real(kind=8) :: epsi
            character(*) :: crit
            integer :: ific
            logical :: llab
            character(*) :: ssigne
          end subroutine utest0
        end interface
