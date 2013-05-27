        interface
          subroutine tbajco(nomta,para,type,nbval,vi,vr,vc,vk,action,&
     &llign)
            character(*) :: nomta
            character(*) :: para
            character(*) :: type
            integer :: nbval
            integer :: vi(*)
            real(kind=8) :: vr(*)
            complex(kind=8) :: vc(*)
            character(*) :: vk(*)
            character(*) :: action
            integer :: llign(*)
          end subroutine tbajco
        end interface
