        interface
          subroutine gcpc(m,in,ip,ac,inpc,ippc,acpc,bf,xp,r,rr,p,irep,&
     &niter,epsi,criter,solveu,matas,smbr,istop,iret)
            integer :: m
            integer :: in(m)
            integer(kind=4) :: ip(*)
            real(kind=8) :: ac(m)
            integer :: inpc(m)
            integer(kind=4) :: ippc(*)
            real(kind=8) :: acpc(m)
            real(kind=8) :: bf(m)
            real(kind=8) :: xp(m)
            real(kind=8) :: r(m)
            real(kind=8) :: rr(m)
            real(kind=8) :: p(m)
            integer :: irep
            integer :: niter
            real(kind=8) :: epsi
            character(len=19) :: criter
            character(len=19) :: solveu
            character(len=19) :: matas
            character(len=19) :: smbr
            integer :: istop
            integer :: iret
          end subroutine gcpc
        end interface
