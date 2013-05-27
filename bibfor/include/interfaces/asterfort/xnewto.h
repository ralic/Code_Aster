        interface
          subroutine xnewto(elp,name,num,nno,ndim,ptint,tabco,jtabls,&
     &ipp,ip,s,itemax,epsmax,ksi)
            integer :: ndim
            character(len=8) :: elp
            character(len=6) :: name
            integer :: num
            integer :: nno
            real(kind=8) :: ptint(*)
            real(kind=8) :: tabco(*)
            integer :: jtabls
            integer :: ipp
            integer :: ip
            real(kind=8) :: s
            integer :: itemax
            real(kind=8) :: epsmax
            real(kind=8) :: ksi(ndim)
          end subroutine xnewto
        end interface
