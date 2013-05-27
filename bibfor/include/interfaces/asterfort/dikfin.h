        interface
          subroutine dikfin(nbt,dnsdu,dnsdt,dmsdt,dnsdu2,dnsdt2,dmsdt2&
     &,ky,kz,krx,krz,klv,klv2)
            integer :: nbt
            real(kind=8) :: dnsdu
            real(kind=8) :: dnsdt
            real(kind=8) :: dmsdt
            real(kind=8) :: dnsdu2
            real(kind=8) :: dnsdt2
            real(kind=8) :: dmsdt2
            real(kind=8) :: ky
            real(kind=8) :: kz
            real(kind=8) :: krx
            real(kind=8) :: krz
            real(kind=8) :: klv(nbt)
            real(kind=8) :: klv2(nbt)
          end subroutine dikfin
        end interface
