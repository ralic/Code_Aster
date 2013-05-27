        interface
          subroutine mmelem(nomte,ndim,nddl,typmae,nne,typmam,nnm,nnl,&
     &nbcps,nbdm,laxis,leltf)
            character(len=16) :: nomte
            integer :: ndim
            integer :: nddl
            character(len=8) :: typmae
            integer :: nne
            character(len=8) :: typmam
            integer :: nnm
            integer :: nnl
            integer :: nbcps
            integer :: nbdm
            logical :: laxis
            logical :: leltf
          end subroutine mmelem
        end interface
