        interface
          subroutine cribif(mod,dsidep,vbifur,nbrac4,racine)
            character(len=8) :: mod
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: vbifur
            integer :: nbrac4
            real(kind=8) :: racine(4)
          end subroutine cribif
        end interface
