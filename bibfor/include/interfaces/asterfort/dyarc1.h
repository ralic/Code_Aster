        interface
          subroutine dyarc1(instc,nbpas,insta,nbinst,arch,epsi,crit)
            real(kind=8) :: instc(*)
            integer :: nbpas
            real(kind=8) :: insta(*)
            integer :: nbinst
            integer :: arch(*)
            real(kind=8) :: epsi
            character(len=8) :: crit
          end subroutine dyarc1
        end interface
