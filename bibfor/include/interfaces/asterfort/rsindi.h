        interface
          subroutine rsindi(tysca,iaobj,paobj,jordr,ival,rval,kval,&
     &cval,epsi,crit,nbordr,nbtrou,nutrou,ndim)
            character(len=4) :: tysca
            integer :: iaobj
            integer :: paobj
            integer :: jordr
            integer :: ival
            real(kind=8) :: rval
            character(*) :: kval
            complex(kind=8) :: cval
            real(kind=8) :: epsi
            character(*) :: crit
            integer :: nbordr
            integer :: nbtrou
            integer :: nutrou(*)
            integer :: ndim
          end subroutine rsindi
        end interface
