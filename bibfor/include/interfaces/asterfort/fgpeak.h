        interface
          subroutine fgpeak(nomfon,pseuil,coemul,nbpoin,valpoi)
            character(*) :: nomfon
            real(kind=8) :: pseuil
            real(kind=8) :: coemul
            integer :: nbpoin
            real(kind=8) :: valpoi(*)
          end subroutine fgpeak
        end interface
