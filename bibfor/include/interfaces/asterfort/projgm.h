        interface
          subroutine projgm(np1,np2,ic,nbm,phii,floc,fmod)
            integer :: np2
            integer :: np1
            integer :: ic
            integer :: nbm
            real(kind=8) :: phii(np2,np1,3)
            real(kind=8) :: floc(*)
            real(kind=8) :: fmod(*)
          end subroutine projgm
        end interface
