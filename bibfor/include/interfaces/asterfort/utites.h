        interface
          subroutine utites(label1,label2,type,nbref,refi,refr,refc,&
     &vali,valr,valc,epsi,crit,ific,llab,ssigne)
            integer :: nbref
            character(*) :: label1
            character(*) :: label2
            character(*) :: type
            integer :: refi(nbref)
            real(kind=8) :: refr(nbref)
            complex(kind=8) :: refc(nbref)
            integer :: vali
            real(kind=8) :: valr
            complex(kind=8) :: valc
            real(kind=8) :: epsi
            character(*) :: crit
            integer :: ific
            logical :: llab
            character(*) :: ssigne
          end subroutine utites
        end interface
