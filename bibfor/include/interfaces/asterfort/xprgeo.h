        interface
          subroutine xprgeo(noma,cnsln,cnslt,grln,grlt,vpoint,cnsbl,&
     &deltat,nodtor,liggrd,cnsbet,listp)
            character(len=8) :: noma
            character(len=19) :: cnsln
            character(len=19) :: cnslt
            character(len=19) :: grln
            character(len=19) :: grlt
            character(len=19) :: vpoint
            character(len=19) :: cnsbl
            real(kind=8) :: deltat
            character(len=19) :: nodtor
            character(len=19) :: liggrd
            character(len=19) :: cnsbet
            character(len=19) :: listp
          end subroutine xprgeo
        end interface
