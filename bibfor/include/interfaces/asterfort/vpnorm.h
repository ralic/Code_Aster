        interface
          subroutine vpnorm(norm,para,lmatr,neq,nbmode,ddlexc,vecpro,&
     &resufr,lmasin,xmastr,isign,numddl,coef)
            integer :: nbmode
            integer :: neq
            character(*) :: norm
            character(*) :: para
            integer :: lmatr
            integer :: ddlexc(*)
            real(kind=8) :: vecpro(neq,*)
            real(kind=8) :: resufr(nbmode,*)
            logical :: lmasin
            real(kind=8) :: xmastr
            integer :: isign
            integer :: numddl
            real(kind=8) :: coef(*)
          end subroutine vpnorm
        end interface
