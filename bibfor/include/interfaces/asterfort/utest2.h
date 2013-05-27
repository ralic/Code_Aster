        interface
          subroutine utest2(cham19,nomail,nonoeu,nupo,nusp,ivari,nocmp&
     &,nbref,tbtxt,refi,refr,refc,typres,epsi,crit,ific,llab,ssigne)
            integer :: nbref
            character(*) :: cham19
            character(*) :: nomail
            character(*) :: nonoeu
            integer :: nupo
            integer :: nusp
            integer :: ivari
            character(*) :: nocmp
            character(len=16) :: tbtxt(2)
            integer :: refi(nbref)
            real(kind=8) :: refr(nbref)
            complex(kind=8) :: refc(nbref)
            character(*) :: typres
            real(kind=8) :: epsi
            character(*) :: crit
            integer :: ific
            logical :: llab
            character(*) :: ssigne
          end subroutine utest2
        end interface
