        interface
          subroutine pidefo(ndim,npg,kpg,compor,fm,epsm,epsp,epsd,&
     &copilo)
            integer :: npg
            integer :: ndim
            integer :: kpg
            character(len=16) :: compor(*)
            real(kind=8) :: fm(3,3)
            real(kind=8) :: epsm(6)
            real(kind=8) :: epsp(6)
            real(kind=8) :: epsd(6)
            real(kind=8) :: copilo(5,npg)
          end subroutine pidefo
        end interface
