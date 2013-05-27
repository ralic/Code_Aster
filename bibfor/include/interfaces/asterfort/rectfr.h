        interface
          subroutine rectfr(nbmode,nbvect,omeshi,npivot,nblagr,valpro,&
     &nvpro,resufi,resufr,nfreq)
            integer :: nfreq
            integer :: nvpro
            integer :: nbmode
            integer :: nbvect
            real(kind=8) :: omeshi
            integer :: npivot
            integer :: nblagr
            real(kind=8) :: valpro(nvpro)
            integer :: resufi(nfreq,*)
            real(kind=8) :: resufr(nfreq,*)
          end subroutine rectfr
        end interface
