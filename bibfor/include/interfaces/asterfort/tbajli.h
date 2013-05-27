        interface
          subroutine tbajli(nomta,nbpar,nompar,vi,vr,vc,vk,nume)
            character(*) :: nomta
            integer :: nbpar
            character(*) :: nompar(*)
            integer :: vi(*)
            real(kind=8) :: vr(*)
            complex(kind=8) :: vc(*)
            character(*) :: vk(*)
            integer :: nume
          end subroutine tbajli
        end interface
