        interface
          subroutine ssdmu1(dref,crit,prec,geo1,geo2,iconf)
            real(kind=8) :: dref
            character(*) :: crit
            real(kind=8) :: prec
            real(kind=8) :: geo1(3)
            real(kind=8) :: geo2(3)
            integer :: iconf
          end subroutine ssdmu1
        end interface
