        interface
          subroutine tbajvr(table,nbpara,nompar,vr,livr)
            character(*) :: table
            integer :: nbpara
            character(*) :: nompar
            real(kind=8) :: vr
            real(kind=8) :: livr(*)
          end subroutine tbajvr
        end interface
