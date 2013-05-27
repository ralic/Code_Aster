        interface
          subroutine nmpoel(nomte,npg,klv,xl,nno,nc,pgl,pgl1,pgl2,ugl,&
     &epsthe,e,em,effm,fl,effl,angs2,rad)
            character(*) :: nomte
            integer :: npg
            real(kind=8) :: klv(*)
            real(kind=8) :: xl
            integer :: nno
            integer :: nc
            real(kind=8) :: pgl(*)
            real(kind=8) :: pgl1(*)
            real(kind=8) :: pgl2(*)
            real(kind=8) :: ugl(*)
            real(kind=8) :: epsthe
            real(kind=8) :: e
            real(kind=8) :: em
            real(kind=8) :: effm(*)
            real(kind=8) :: fl(*)
            real(kind=8) :: effl(*)
            real(kind=8) :: angs2
            real(kind=8) :: rad
          end subroutine nmpoel
        end interface
