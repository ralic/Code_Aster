        interface
          subroutine spephy(ioptch,intphy,intmod,nomu,table,freq,cham,&
     &specmr,specmi,disc,nnoe,nomcmp,nuor,nbmr,nbn,imod1,nbpf,nbm,ivitef&
     &)
            integer :: nbm
            integer :: nbpf
            integer :: nbn
            integer :: nbmr
            integer :: ioptch
            logical :: intphy
            logical :: intmod
            character(len=8) :: nomu
            character(len=8) :: table
            real(kind=8) :: freq(2,nbm,*)
            real(kind=8) :: cham(nbn,nbmr)
            real(kind=8) :: specmr(nbpf,*)
            real(kind=8) :: specmi(nbpf,*)
            real(kind=8) :: disc(*)
            character(len=8) :: nnoe(nbn)
            character(len=8) :: nomcmp
            integer :: nuor(nbmr)
            integer :: imod1
            integer :: ivitef
          end subroutine spephy
        end interface
