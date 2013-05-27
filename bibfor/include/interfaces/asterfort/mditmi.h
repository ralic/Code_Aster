        interface
          subroutine mditmi(typflu,nombm,icoupl,nbm0,nbmode,nbmd,vgap,&
     &itrans,eps,ts,nts,itypfl)
            character(len=8) :: typflu
            character(len=8) :: nombm
            integer :: icoupl
            integer :: nbm0
            integer :: nbmode
            integer :: nbmd
            real(kind=8) :: vgap
            integer :: itrans
            real(kind=8) :: eps
            real(kind=8) :: ts
            integer :: nts
            integer :: itypfl
          end subroutine mditmi
        end interface
