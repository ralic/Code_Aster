        interface
          subroutine adimeq(nbm,nbmc,cmod,kmod,fmod,masgi,amori,pulsi,&
     &cmodc,kmodc,fmoda)
            integer :: nbm
            integer :: nbmc
            real(kind=8) :: cmod(nbm,*)
            real(kind=8) :: kmod(nbm,*)
            real(kind=8) :: fmod(*)
            real(kind=8) :: masgi(*)
            real(kind=8) :: amori(*)
            real(kind=8) :: pulsi(*)
            real(kind=8) :: cmodc(nbm,*)
            real(kind=8) :: kmodc(nbm,*)
            real(kind=8) :: fmoda(*)
          end subroutine adimeq
        end interface
