        interface
          subroutine rrssm2(neq,smhcr,smhci,smdir,smdii,idlexc,coef,&
     &valmi,valmr)
            integer :: neq
            integer(kind=4) :: smhcr(*)
            integer(kind=4) :: smhci(*)
            integer :: smdir(*)
            integer :: smdii(*)
            integer :: idlexc(*)
            real(kind=8) :: coef
            real(kind=8) :: valmi(*)
            real(kind=8) :: valmr(*)
          end subroutine rrssm2
        end interface
