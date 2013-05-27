        interface
          subroutine pjcorr(nomo2,chbid,cns1z,ces2z,ligrel,corres,&
     &option,nompar,iret)
            character(len=8) :: nomo2
            character(len=19) :: chbid
            character(*) :: cns1z
            character(*) :: ces2z
            character(len=19) :: ligrel
            character(len=16) :: corres
            character(len=16) :: option
            character(len=8) :: nompar
            integer :: iret
          end subroutine pjcorr
        end interface
