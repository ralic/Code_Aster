        interface
          subroutine rsadpa(nomsd,cel,npara,lpara,iordr,itype,ljeveu,&
     &ctype)
            character(*) :: nomsd
            character(len=1) :: cel
            integer :: npara
            character(*) :: lpara(*)
            integer :: iordr
            integer :: itype
            integer :: ljeveu(*)
            character(*) :: ctype(*)
          end subroutine rsadpa
        end interface
