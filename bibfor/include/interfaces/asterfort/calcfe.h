        interface
          subroutine calcfe(nr,ndt,nvi,vind,df,gamsns,fe,fp,iret)
            integer :: nr
            integer :: ndt
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: df(3,3)
            real(kind=8) :: gamsns(3,3)
            real(kind=8) :: fe(3,3)
            real(kind=8) :: fp(3,3)
            integer :: iret
          end subroutine calcfe
        end interface
