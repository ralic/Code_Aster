        interface
          subroutine radipg(sig1,sig2,npg,nbsig,radia,cosang,ind,&
     &compor,imate,nvi,vari1,vari2)
            real(kind=8) :: sig1(*)
            real(kind=8) :: sig2(*)
            integer :: npg
            integer :: nbsig
            real(kind=8) :: radia(*)
            real(kind=8) :: cosang(*)
            integer :: ind
            character(len=16) :: compor
            integer :: imate
            integer :: nvi
            real(kind=8) :: vari1(*)
            real(kind=8) :: vari2(*)
          end subroutine radipg
        end interface
