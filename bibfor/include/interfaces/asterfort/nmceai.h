        interface
          subroutine nmceai(numedd,depdel,deppr1,deppr2,depold,sdpilo,&
     &rho,eta,isxfe,f,indic)
            character(len=24) :: numedd
            character(len=19) :: depdel
            character(len=19) :: deppr1
            character(len=19) :: deppr2
            character(len=19) :: depold
            character(len=19) :: sdpilo
            real(kind=8) :: rho
            real(kind=8) :: eta
            logical :: isxfe
            real(kind=8) :: f
            integer :: indic
          end subroutine nmceai
        end interface
