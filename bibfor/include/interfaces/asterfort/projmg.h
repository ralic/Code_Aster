        interface
          subroutine projmg(np1,np2,ic,nbm,phii,depg,xglo)
            integer :: np2
            integer :: np1
            integer :: ic
            integer :: nbm
            real(kind=8) :: phii(np2,np1,*)
            real(kind=8) :: depg(*)
            real(kind=8) :: xglo(*)
          end subroutine projmg
        end interface
