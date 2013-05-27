        interface
          subroutine utest1(chamgd,typtes,typres,nbref,tbtxt,refi,refr&
     &,refc,epsi,crit,ific,llab,ssigne)
            integer :: nbref
            character(*) :: chamgd
            character(len=8) :: typtes
            character(*) :: typres
            character(len=16) :: tbtxt(2)
            integer :: refi(nbref)
            real(kind=8) :: refr(nbref)
            complex(kind=8) :: refc(nbref)
            real(kind=8) :: epsi
            character(*) :: crit
            integer :: ific
            logical :: llab
            character(*) :: ssigne
          end subroutine utest1
        end interface
