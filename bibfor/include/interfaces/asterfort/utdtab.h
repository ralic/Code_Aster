        interface
          subroutine utdtab(raz,na,nb,mb,md,a,b,d,xab,dtab)
            integer :: md
            integer :: mb
            integer :: nb
            integer :: na
            character(*) :: raz
            real(kind=8) :: a(na,nb)
            real(kind=8) :: b(nb,mb)
            real(kind=8) :: d(na,md)
            real(kind=8) :: xab(na,mb)
            real(kind=8) :: dtab(md,mb)
          end subroutine utdtab
        end interface
