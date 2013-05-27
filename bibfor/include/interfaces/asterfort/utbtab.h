        interface
          subroutine utbtab(raz,na,mb,a,b,xab,btab)
            integer :: mb
            integer :: na
            character(*) :: raz
            real(kind=8) :: a(na,na)
            real(kind=8) :: b(na,mb)
            real(kind=8) :: xab(na,mb)
            real(kind=8) :: btab(mb,mb)
          end subroutine utbtab
        end interface
