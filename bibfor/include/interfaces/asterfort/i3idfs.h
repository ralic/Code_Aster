        interface
          subroutine i3idfs(epsi,k,f,nba,sgt,coorsm,nbpt,lstpt,fink)
            real(kind=8) :: epsi
            integer :: k
            integer :: f
            integer :: nba
            real(kind=8) :: sgt(*)
            real(kind=8) :: coorsm(3,*)
            integer :: nbpt
            integer :: lstpt(*)
            logical :: fink
          end subroutine i3idfs
        end interface
