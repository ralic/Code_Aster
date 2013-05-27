        interface
          subroutine xprtor(method,model,noma,cnxinv,fispre,fiss,vcn,&
     &grlr,cnsln,grln,cnslt,grlt,tore,radtor,radimp,cnsdis,disfr,cnsbl,&
     &nodcal,elecal,liggrd,vcnt,grlrt)
            character(len=8) :: method
            character(len=8) :: model
            character(len=8) :: noma
            character(len=19) :: cnxinv
            character(len=8) :: fispre
            character(len=8) :: fiss
            character(len=24) :: vcn
            character(len=24) :: grlr
            character(len=19) :: cnsln
            character(len=19) :: grln
            character(len=19) :: cnslt
            character(len=19) :: grlt
            logical :: tore
            real(kind=8) :: radtor
            real(kind=8) :: radimp
            character(len=19) :: cnsdis
            character(len=19) :: disfr
            character(len=19) :: cnsbl
            character(len=19) :: nodcal
            character(len=19) :: elecal
            character(len=19) :: liggrd
            character(len=24) :: vcnt
            character(len=24) :: grlrt
          end subroutine xprtor
        end interface
