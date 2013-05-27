        interface
          subroutine dxglrc(nomte,opt,compor,xyzl,ul,dul,btsig,ktan,&
     &pgl,crit,codret)
            character(len=16) :: nomte
            character(len=16) :: opt
            character(len=16) :: compor(*)
            real(kind=8) :: xyzl(3,4)
            real(kind=8) :: ul(6,4)
            real(kind=8) :: dul(6,4)
            real(kind=8) :: btsig(6,4)
            real(kind=8) :: ktan(300)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: crit(*)
            integer :: codret
          end subroutine dxglrc
        end interface
