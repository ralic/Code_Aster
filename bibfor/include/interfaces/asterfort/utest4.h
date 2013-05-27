        interface
          subroutine utest4(chamgd,typtes,typres,nbref,tbtxt,refi,refr&
     &,refc,epsi,lign1,lign2,crit,ific,nbcmp,nocmp,llab,ssigne)
            integer :: nbref
            character(*) :: chamgd
            character(len=8) :: typtes
            character(*) :: typres
            character(len=16) :: tbtxt(2)
            integer :: refi(nbref)
            real(kind=8) :: refr(nbref)
            complex(kind=8) :: refc(nbref)
            real(kind=8) :: epsi
            character(len=200) :: lign1
            character(len=200) :: lign2
            character(*) :: crit
            integer :: ific
            integer :: nbcmp
            character(len=8) :: nocmp(*)
            logical :: llab
            character(*) :: ssigne
          end subroutine utest4
        end interface
