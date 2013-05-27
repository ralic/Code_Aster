        interface
          subroutine cvmini(typess,essai,mod,nmat,materf,timed,timef,&
     &yd,epsd,deps,dy)
            integer :: nmat
            integer :: typess
            real(kind=8) :: essai
            character(len=8) :: mod
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yd(*)
            real(kind=8) :: epsd(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: dy(*)
          end subroutine cvmini
        end interface
