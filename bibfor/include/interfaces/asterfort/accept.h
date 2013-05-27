        interface
          subroutine accept(f,nbm,method,imode,jmode,uflui,jc,dir,uc,&
     &uct,l,lt)
            real(kind=8) :: f
            integer :: nbm
            character(len=8) :: method
            integer :: imode
            integer :: jmode
            real(kind=8) :: uflui
            real(kind=8) :: jc
            real(kind=8) :: dir(3,3)
            real(kind=8) :: uc
            real(kind=8) :: uct
            real(kind=8) :: l
            real(kind=8) :: lt
          end subroutine accept
        end interface
