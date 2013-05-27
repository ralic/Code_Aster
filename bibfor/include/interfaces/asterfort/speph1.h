        interface
          subroutine speph1(intphy,intmod,nomu,cham,specmr,specmi,nnoe&
     &,nomcmp,nbmode,nbn,nbpf)
            integer :: nbpf
            integer :: nbn
            logical :: intphy
            logical :: intmod
            character(len=8) :: nomu
            real(kind=8) :: cham(nbn,*)
            real(kind=8) :: specmr(nbpf,*)
            real(kind=8) :: specmi(nbpf,*)
            character(len=8) :: nnoe(*)
            character(len=8) :: nomcmp(*)
            integer :: nbmode
          end subroutine speph1
        end interface
