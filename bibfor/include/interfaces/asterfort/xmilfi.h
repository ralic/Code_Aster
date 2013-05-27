        interface
          subroutine xmilfi(elp,ndim,nno,ptint,jtabco,jtabls,ipp,ip,&
     &milfi)
            integer :: ndim
            character(len=8) :: elp
            integer :: nno
            real(kind=8) :: ptint(*)
            integer :: jtabco
            integer :: jtabls
            integer :: ipp
            integer :: ip
            real(kind=8) :: milfi(ndim)
          end subroutine xmilfi
        end interface
