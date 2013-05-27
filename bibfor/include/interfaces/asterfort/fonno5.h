        interface
          subroutine fonno5(noma,indic,nbnoff,noe,na,nb,ndim,nbnoel,&
     &indr,vnor,vdir)
            character(len=8) :: noma
            integer :: indic(4)
            integer :: nbnoff
            integer :: noe(4,4)
            integer :: na
            integer :: nb
            integer :: ndim
            integer :: nbnoel
            integer :: indr(2)
            real(kind=8) :: vnor(2,3)
            real(kind=8) :: vdir(2,3)
          end subroutine fonno5
        end interface
