        interface
          subroutine xmulco(contac,ddlc,ddlm,iaint,ifiss,jheano,vstnc,&
     &lact,lcalel,lelim,ndim,nfe,nfh,nfiss,ninter,nlact,nno,nnol,nnom,&
     &nnos,pla,typma)
            integer :: contac
            integer :: ddlc
            integer :: ddlm
            integer :: iaint
            integer :: ifiss
            integer :: jheano
            integer :: vstnc(*)
            integer :: lact(8)
            logical :: lcalel
            logical :: lelim
            integer :: ndim
            integer :: nfe
            integer :: nfh
            integer :: nfiss
            integer :: ninter
            integer :: nlact
            integer :: nno
            integer :: nnol
            integer :: nnom
            integer :: nnos
            integer :: pla(27)
            character(len=8) :: typma
          end subroutine xmulco
        end interface
