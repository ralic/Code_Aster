        interface
          subroutine dktnli(nomte,opt,xyzl,ul,dul,btsig,ktan,codret)
            character(len=16) :: nomte
            character(len=16) :: opt
            real(kind=8) :: xyzl(3,*)
            real(kind=8) :: ul(6,*)
            real(kind=8) :: dul(6,*)
            real(kind=8) :: btsig(6,*)
            real(kind=8) :: ktan(*)
            integer :: codret
          end subroutine dktnli
        end interface
