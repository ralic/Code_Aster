        interface
          subroutine defcur(vecr1,veck1,nb,vecr2,nv,nommai,nm,prolgd,&
     &interp)
            integer :: nv
            integer :: nb
            real(kind=8) :: vecr1(nb)
            character(len=8) :: veck1(nb)
            real(kind=8) :: vecr2(nv)
            character(len=8) :: nommai
            integer :: nm
            character(len=2) :: prolgd
            character(len=8) :: interp
          end subroutine defcur
        end interface
