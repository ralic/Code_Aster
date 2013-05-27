        interface
          subroutine tbnuli(tabin,npacri,lipacr,vi,vr,vc,vk,lprec,&
     &lcrit,nume)
            character(*) :: tabin
            integer :: npacri
            character(*) :: lipacr(*)
            integer :: vi(*)
            real(kind=8) :: vr(*)
            complex(kind=8) :: vc(*)
            character(*) :: vk(*)
            real(kind=8) :: lprec(*)
            character(*) :: lcrit(*)
            integer :: nume
          end subroutine tbnuli
        end interface
