        interface
          subroutine btsig(lonlig,loncol,jacgau,bmat,sigma,bsigma)
            integer :: loncol
            integer :: lonlig
            real(kind=8) :: jacgau
            real(kind=8) :: bmat(loncol,1)
            real(kind=8) :: sigma(1)
            real(kind=8) :: bsigma(1)
          end subroutine btsig
        end interface
