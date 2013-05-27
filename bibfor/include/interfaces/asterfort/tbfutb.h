        interface
          subroutine tbfutb(tabout,basout,ntab,ltabin,para,typpar,vi,&
     &vr,vc,vk)
            character(*) :: tabout
            character(*) :: basout
            integer :: ntab
            character(*) :: ltabin(*)
            character(*) :: para
            character(*) :: typpar
            integer :: vi(*)
            real(kind=8) :: vr(*)
            complex(kind=8) :: vc(*)
            character(*) :: vk(*)
          end subroutine tbfutb
        end interface
