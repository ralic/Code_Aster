        interface
          subroutine xrell2(tabnoz,ndim,narz,tabcoz,tabcrz,lgroup,&
     &nliseq)
            integer :: narz
            integer :: ndim
            integer :: tabnoz(3,narz)
            real(kind=8) :: tabcoz(ndim,narz)
            real(kind=8) :: tabcrz(narz)
            logical :: lgroup
            character(len=19) :: nliseq
          end subroutine xrell2
        end interface
