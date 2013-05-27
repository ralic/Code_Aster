        interface
          subroutine utctab(raz,na,mb,mc,a,b,c,xab,ctab)
            integer :: mc
            integer :: mb
            integer :: na
            character(*) :: raz
            real(kind=8) :: a(na,na)
            real(kind=8) :: b(na,mb)
            real(kind=8) :: c(na,mc)
            real(kind=8) :: xab(na,mb)
            real(kind=8) :: ctab(mc,mb)
          end subroutine utctab
        end interface
