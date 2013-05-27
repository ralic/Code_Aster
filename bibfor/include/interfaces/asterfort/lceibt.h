        interface
          subroutine lceibt(ndimsi,eps,epsf,dep,invn,cn,dsidep)
            integer :: ndimsi
            real(kind=8) :: eps(6)
            real(kind=8) :: epsf(6)
            real(kind=8) :: dep(6,12)
            real(kind=8) :: invn(6,6)
            real(kind=8) :: cn(6,6)
            real(kind=8) :: dsidep(6,6)
          end subroutine lceibt
        end interface
