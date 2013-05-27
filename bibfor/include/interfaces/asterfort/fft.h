        interface
          subroutine fft(s,n,ifft)
            integer :: n
            complex(kind=8) :: s(n)
            integer :: ifft
          end subroutine fft
        end interface
