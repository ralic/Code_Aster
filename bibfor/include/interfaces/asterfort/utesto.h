        interface
          subroutine utesto(nomobj,type,tbtxt,refi,refr,epsi,crit,ific&
     &,llab,ssigne)
            character(len=24) :: nomobj
            character(*) :: type
            character(len=16) :: tbtxt(2)
            integer :: refi
            real(kind=8) :: refr
            real(kind=8) :: epsi
            character(*) :: crit
            integer :: ific
            logical :: llab
            character(*) :: ssigne
          end subroutine utesto
        end interface
