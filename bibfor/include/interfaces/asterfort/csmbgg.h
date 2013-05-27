        interface
          subroutine csmbgg(lmat,vsmb,vcine,cvsmb,cvcine,type)
            integer :: lmat
            real(kind=8) :: vsmb(*)
            real(kind=8) :: vcine(*)
            complex(kind=8) :: cvsmb(*)
            complex(kind=8) :: cvcine(*)
            character(*) :: type
          end subroutine csmbgg
        end interface
