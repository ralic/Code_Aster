        interface
          function critnu(zimat,nmnbn,deps,dtg,normm)
            integer :: zimat
            real(kind=8) :: nmnbn(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: dtg(6,6)
            real(kind=8) :: normm
            integer :: critnu
          end function critnu
        end interface
