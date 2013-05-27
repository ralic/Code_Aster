        interface
          subroutine affori(typ,nomt,cara,val,jad,jdno,jdco,ivr,nutyma&
     &,ntseg,carori,nco,ier)
            integer :: nco
            character(*) :: typ
            character(*) :: nomt
            character(*) :: cara
            real(kind=8) :: val(6)
            integer :: jad
            integer :: jdno
            integer :: jdco
            integer :: ivr(*)
            integer :: nutyma
            integer :: ntseg
            character(*) :: carori(nco)
            integer :: ier
          end subroutine affori
        end interface
