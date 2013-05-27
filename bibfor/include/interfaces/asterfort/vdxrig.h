        interface
          subroutine vdxrig(nomte,xi,rig,nb1,indm,indf)
            character(len=16) :: nomte
            real(kind=8) :: xi(3,9)
            real(kind=8) :: rig(51,51)
            integer :: nb1
            integer :: indm
            integer :: indf
          end subroutine vdxrig
        end interface
