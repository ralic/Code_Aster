        interface
          subroutine tbajva(table,nbpara,nompar,vi,livi,vr,livr,vc,&
     &livc,vk,livk)
            character(*) :: table
            integer :: nbpara
            character(*) :: nompar
            integer :: vi
            integer :: livi(*)
            real(kind=8) :: vr
            real(kind=8) :: livr(*)
            complex(kind=8) :: vc
            complex(kind=8) :: livc(*)
            character(*) :: vk
            character(*) :: livk(*)
          end subroutine tbajva
        end interface
