        interface
          function edgequ(ndimsi,tens,ani)
            integer :: ndimsi
            real(kind=8) :: tens(ndimsi)
            real(kind=8) :: ani(6,6)
            real(kind=8) :: edgequ
          end function edgequ
        end interface
