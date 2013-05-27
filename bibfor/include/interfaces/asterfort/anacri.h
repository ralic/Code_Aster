        interface
          subroutine anacri(nomcri,nomfor,typcha,impgrd,paract,fordef,&
     &crsigm,crepst,crepse,crepsp)
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            character(len=16) :: typcha
            character(len=3) :: impgrd
            integer :: paract(30)
            logical :: fordef
            logical :: crsigm
            logical :: crepst
            logical :: crepse
            logical :: crepsp
          end subroutine anacri
        end interface
