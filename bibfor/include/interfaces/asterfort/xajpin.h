        interface
          subroutine xajpin(ndim,list,long,ipt,cpt,newpt,longar,ainter&
     &,ia,in,al)
            integer :: ndim
            real(kind=8) :: list(*)
            integer :: long
            integer :: ipt
            integer :: cpt
            real(kind=8) :: newpt(3)
            real(kind=8) :: longar
            real(kind=8) :: ainter(*)
            integer :: ia
            integer :: in
            real(kind=8) :: al
          end subroutine xajpin
        end interface
