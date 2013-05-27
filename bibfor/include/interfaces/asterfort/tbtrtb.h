        interface
          subroutine tbtrtb(tabin,basout,tabout,npara,lipara,lcrit,&
     &prec,crit)
            character(*) :: tabin
            character(*) :: basout
            character(*) :: tabout
            integer :: npara
            character(*) :: lipara(*)
            character(*) :: lcrit(*)
            real(kind=8) :: prec
            character(len=8) :: crit
          end subroutine tbtrtb
        end interface
