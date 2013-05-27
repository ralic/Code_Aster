        interface
          subroutine rotama(geomi,pt,d,angl,bidim)
            character(len=19) :: geomi
            real(kind=8) :: pt(3)
            real(kind=8) :: d(3)
            real(kind=8) :: angl
            logical :: bidim
          end subroutine rotama
        end interface
