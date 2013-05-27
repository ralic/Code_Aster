        interface
          subroutine fonno6(resu,noma,ndim,ina,nbnose,iseg,nseg,noe,&
     &indr,nbnoel,vnor,vdir,basseg,vect,sens)
            integer :: ndim
            character(len=8) :: resu
            character(len=8) :: noma
            integer :: ina
            integer :: nbnose
            integer :: iseg
            integer :: nseg
            integer :: noe(4,4)
            integer :: indr(2)
            integer :: nbnoel
            real(kind=8) :: vnor(2,3)
            real(kind=8) :: vdir(2,3)
            character(len=19) :: basseg
            real(kind=8) :: vect(3)
            real(kind=8) :: sens
          end subroutine fonno6
        end interface
