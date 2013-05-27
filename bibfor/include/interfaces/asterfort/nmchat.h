        interface
          subroutine nmchat(matel,mat,nbvar,memo,visc,plast,sigmdv,&
     &depsdv,pm,dp,ndimsi,dt,rpvp,qp,vim,idelta,n1,n2,beta1,beta2,dsidep&
     &)
            real(kind=8) :: matel(*)
            real(kind=8) :: mat(*)
            integer :: nbvar
            integer :: memo
            integer :: visc
            real(kind=8) :: plast
            real(kind=8) :: sigmdv(6)
            real(kind=8) :: depsdv(6)
            real(kind=8) :: pm
            real(kind=8) :: dp
            integer :: ndimsi
            real(kind=8) :: dt
            real(kind=8) :: rpvp
            real(kind=8) :: qp
            real(kind=8) :: vim(*)
            integer :: idelta
            real(kind=8) :: n1
            real(kind=8) :: n2
            real(kind=8) :: beta1
            real(kind=8) :: beta2
            real(kind=8) :: dsidep(6,6)
          end subroutine nmchat
        end interface
