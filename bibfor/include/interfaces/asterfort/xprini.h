        interface
          subroutine xprini(model,noma,cnxinv,grille,fispre,fiss,cnsln&
     &,cnslt,cnsgls,noesom,noresi,vcn,grlr,lcmin)
            character(len=8) :: model
            character(len=8) :: noma
            character(len=19) :: cnxinv
            logical :: grille
            character(len=8) :: fispre
            character(len=8) :: fiss
            character(len=19) :: cnsln
            character(len=19) :: cnslt
            character(len=19) :: cnsgls
            character(len=19) :: noesom
            character(len=19) :: noresi
            character(len=24) :: vcn
            character(len=24) :: grlr
            real(kind=8) :: lcmin
          end subroutine xprini
        end interface
