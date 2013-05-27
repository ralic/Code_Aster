        interface
          subroutine wpnorm(norm,para,lmatr,neq,nbmode,ddlexc,vecpro,&
     &resufr,coef)
            integer :: nbmode
            integer :: neq
            character(*) :: norm
            character(*) :: para
            integer :: lmatr(*)
            integer :: ddlexc(*)
            complex(kind=8) :: vecpro(neq,*)
            real(kind=8) :: resufr(nbmode,*)
            real(kind=8) :: coef(*)
          end subroutine wpnorm
        end interface
