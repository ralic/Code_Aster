        interface
          subroutine dxmat2(pgl,icou,npg,ordi,epi,epais,dm,indith)
            real(kind=8) :: pgl(3,3)
            integer :: icou
            integer :: npg
            real(kind=8) :: ordi
            real(kind=8) :: epi
            real(kind=8) :: epais
            real(kind=8) :: dm(3,3)
            integer :: indith
          end subroutine dxmat2
        end interface
