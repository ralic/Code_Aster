        interface
          subroutine vechme(stop,modelz,chargz,infchz,inst,carele,mate&
     &,vrcplu,ligrez,vecelz)
            character(len=1) :: stop
            character(*) :: modelz
            character(*) :: chargz
            character(*) :: infchz
            real(kind=8) :: inst(3)
            character(*) :: carele
            character(*) :: mate
            character(*) :: vrcplu
            character(*) :: ligrez
            character(*) :: vecelz
          end subroutine vechme
        end interface
