        interface
          subroutine mdmasf(i,dnorm,masgen,nbmode,phicar,fexgen,accgen&
     &,puls2,amogen,coefa)
            integer :: i
            real(kind=8) :: dnorm
            real(kind=8) :: masgen(*)
            integer :: nbmode
            real(kind=8) :: phicar(*)
            real(kind=8) :: fexgen(*)
            real(kind=8) :: accgen(*)
            real(kind=8) :: puls2(*)
            real(kind=8) :: amogen(*)
            real(kind=8) :: coefa
          end subroutine mdmasf
        end interface
