        interface
          subroutine excent(sens,excen,nbpoin,nbcmp,lreel,reffin,&
     &reffou,ceffin,ceffou)
            character(*) :: sens
            real(kind=8) :: excen
            integer :: nbpoin
            integer :: nbcmp
            logical :: lreel
            real(kind=8) :: reffin(*)
            real(kind=8) :: reffou(*)
            complex(kind=8) :: ceffin(*)
            complex(kind=8) :: ceffou(*)
          end subroutine excent
        end interface
