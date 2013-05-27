        interface
          subroutine vpcntl(cty,mode,option,omemin,omemax,seuil,nfreq,&
     &ipos,lmat,omecor,precdc,ier,vpinf,vpmax,freq,err,charge,typres,&
     &nblagr,solveu,nbrssa,precsh)
            integer :: nfreq
            character(len=1) :: cty
            character(*) :: mode
            character(*) :: option
            real(kind=8) :: omemin
            real(kind=8) :: omemax
            real(kind=8) :: seuil
            integer :: ipos(*)
            integer :: lmat(3)
            real(kind=8) :: omecor
            real(kind=8) :: precdc
            integer :: ier
            real(kind=8) :: vpinf
            real(kind=8) :: vpmax
            real(kind=8) :: freq(nfreq)
            real(kind=8) :: err(nfreq)
            real(kind=8) :: charge(nfreq)
            character(*) :: typres
            integer :: nblagr
            character(len=19) :: solveu
            integer :: nbrssa
            real(kind=8) :: precsh
          end subroutine vpcntl
        end interface
