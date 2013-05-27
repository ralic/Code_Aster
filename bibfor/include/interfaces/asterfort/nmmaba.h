        interface
          subroutine nmmaba(icodma,compor,e,dsde,sigy,ncstpm,cstpm)
            integer :: ncstpm
            integer :: icodma
            character(len=16) :: compor
            real(kind=8) :: e
            real(kind=8) :: dsde
            real(kind=8) :: sigy
            real(kind=8) :: cstpm(ncstpm)
          end subroutine nmmaba
        end interface
