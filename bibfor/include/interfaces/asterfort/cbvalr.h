        interface
          subroutine cbvalr(rouc,neq,smhc,smdi,idlexc,coefr,coefc,&
     &valmi,valmr,valmc)
            character(len=2) :: rouc
            integer :: neq
            integer(kind=4) :: smhc(*)
            integer :: smdi(*)
            integer :: idlexc(*)
            real(kind=8) :: coefr
            complex(kind=8) :: coefc
            real(kind=8) :: valmi(*)
            real(kind=8) :: valmr(*)
            complex(kind=8) :: valmc(*)
          end subroutine cbvalr
        end interface
