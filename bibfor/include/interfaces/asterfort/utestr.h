        interface
          subroutine utestr(cham19,nonoeu,nocmp,nbref,tbtxt,refi,refr,&
     &refc,typres,epsi,crit,ific,llab,ssigne)
            integer :: nbref
            character(len=19) :: cham19
            character(len=33) :: nonoeu
            character(len=8) :: nocmp
            character(len=16) :: tbtxt(2)
            integer :: refi(nbref)
            real(kind=8) :: refr(nbref)
            complex(kind=8) :: refc(nbref)
            character(len=1) :: typres
            real(kind=8) :: epsi
            character(*) :: crit
            integer :: ific
            logical :: llab
            character(*) :: ssigne
          end subroutine utestr
        end interface
