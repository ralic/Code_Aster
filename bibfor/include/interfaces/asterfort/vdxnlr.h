        interface
          subroutine vdxnlr(option,nomte,xi,rig,nb1,codret)
            character(len=16) :: option
            character(len=16) :: nomte
            real(kind=8) :: xi(3,9)
            real(kind=8) :: rig(51,51)
            integer :: nb1
            integer :: codret
          end subroutine vdxnlr
        end interface
