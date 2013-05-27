        interface
          subroutine inipan(np1,nbm,cmod0,kmod0,cmod,kmod,amor0,puls0,&
     &amor,puls,fnlmod,fexmod,fmod00)
            integer :: np1
            integer :: nbm
            real(kind=8) :: cmod0(np1,*)
            real(kind=8) :: kmod0(np1,*)
            real(kind=8) :: cmod(np1,*)
            real(kind=8) :: kmod(np1,*)
            real(kind=8) :: amor0(*)
            real(kind=8) :: puls0(*)
            real(kind=8) :: amor(*)
            real(kind=8) :: puls(*)
            real(kind=8) :: fnlmod(*)
            real(kind=8) :: fexmod(*)
            real(kind=8) :: fmod00(*)
          end subroutine inipan
        end interface
