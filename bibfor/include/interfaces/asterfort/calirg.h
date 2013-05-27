        interface
          subroutine calirg(motfac,iocc,ndim,noma,lnuno2,geom2,mrota,&
     &lrota)
            character(*) :: motfac
            integer :: iocc
            integer :: ndim
            character(len=8) :: noma
            character(*) :: lnuno2
            character(*) :: geom2
            real(kind=8) :: mrota(3,3)
            logical :: lrota
          end subroutine calirg
        end interface
