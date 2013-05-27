        interface
          subroutine brdsde(e0,nu0,dsidep,vim,sigm)
            real(kind=8) :: e0
            real(kind=8) :: nu0
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: vim(65)
            real(kind=8) :: sigm(6)
          end subroutine brdsde
        end interface
