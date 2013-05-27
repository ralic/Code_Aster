        interface
          subroutine foc1su(sortie,nbfon,nomfon,coef,coefz,type,ccplx,&
     &fcplx,lpara,base)
            character(*) :: sortie
            integer :: nbfon
            character(*) :: nomfon(*)
            real(kind=8) :: coef(*)
            complex(kind=8) :: coefz(*)
            character(*) :: type
            logical :: ccplx(*)
            logical :: fcplx(*)
            character(*) :: lpara
            character(len=1) :: base
          end subroutine foc1su
        end interface
