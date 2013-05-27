        interface
          subroutine posthm(option,modint,jgano,ncmp,nvim,vpg,vno)
            character(len=16) :: option
            character(len=3) :: modint
            integer :: jgano
            integer :: ncmp
            integer :: nvim
            real(kind=8) :: vpg(*)
            real(kind=8) :: vno(*)
          end subroutine posthm
        end interface
