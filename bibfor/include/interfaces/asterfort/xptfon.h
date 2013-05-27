        interface
          subroutine xptfon(noma,ndim,nmafon,cnslt,cnsln,cnxinv,jmafon&
     &,nxptff,jfon,nfon,jbas,jtail,fiss,goinop,listpt,orient)
            character(len=8) :: noma
            integer :: ndim
            integer :: nmafon
            character(len=19) :: cnslt
            character(len=19) :: cnsln
            character(len=19) :: cnxinv
            integer :: jmafon
            integer :: nxptff
            integer :: jfon
            integer :: nfon
            integer :: jbas
            integer :: jtail
            character(len=8) :: fiss
            logical :: goinop
            character(len=19) :: listpt
            logical :: orient
          end subroutine xptfon
        end interface
