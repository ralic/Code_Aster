        interface
          subroutine rsorac(nomsd,acces,ival,rval,kval,cval,epsi,crit,&
     &nutrou,ndim,nbtrou)
            character(*) :: nomsd
            character(*) :: acces
            integer :: ival
            real(kind=8) :: rval
            character(*) :: kval
            complex(kind=8) :: cval
            real(kind=8) :: epsi
            character(*) :: crit
            integer :: nutrou(*)
            integer :: ndim
            integer :: nbtrou
          end subroutine rsorac
        end interface
