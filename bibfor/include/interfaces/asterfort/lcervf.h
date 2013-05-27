        interface
          subroutine lcervf(mode,ndimsi,eps,treps,epsdv,gameps,dgamde)
            integer :: ndimsi
            integer :: mode
            real(kind=8) :: eps(ndimsi)
            real(kind=8) :: treps
            real(kind=8) :: epsdv(ndimsi)
            real(kind=8) :: gameps
            real(kind=8) :: dgamde(ndimsi)
          end subroutine lcervf
        end interface
