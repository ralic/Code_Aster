        interface
          subroutine calfmn(np1,nbm,testc,fmod0,fmod00,cmod,kmod,vitg0&
     &,depg0)
            integer :: np1
            integer :: nbm
            integer :: testc
            real(kind=8) :: fmod0(*)
            real(kind=8) :: fmod00(*)
            real(kind=8) :: cmod(np1,*)
            real(kind=8) :: kmod(np1,*)
            real(kind=8) :: vitg0(*)
            real(kind=8) :: depg0(*)
          end subroutine calfmn
        end interface
