        interface
          subroutine tbextb(tabin,basout,tabout,npacri,lipacr,lcrpa,vi&
     &,vr,vc,vk,lprec,lcrit,iret)
            character(*) :: tabin
            character(*) :: basout
            character(*) :: tabout
            integer :: npacri
            character(*) :: lipacr(*)
            character(*) :: lcrpa(*)
            integer :: vi(*)
            real(kind=8) :: vr(*)
            complex(kind=8) :: vc(*)
            character(*) :: vk(*)
            real(kind=8) :: lprec(*)
            character(*) :: lcrit(*)
            integer :: iret
          end subroutine tbextb
        end interface
