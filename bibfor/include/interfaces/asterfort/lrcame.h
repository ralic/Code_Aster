        interface
          subroutine lrcame(nrofic,nochmd,nomamd,nomaas,ligrel,option,&
     &param,typech,typen,npgma,npgmm,nbcmpv,ncmpva,ncmpvm,iinst,numpt,&
     &numord,inst,crit,prec,nomgd,ncmprf,jnocmp,chames,codret)
            integer :: nrofic
            character(*) :: nochmd
            character(*) :: nomamd
            character(len=8) :: nomaas
            character(len=19) :: ligrel
            character(len=24) :: option
            character(len=8) :: param
            character(*) :: typech
            integer :: typen
            integer :: npgma(*)
            integer :: npgmm(*)
            integer :: nbcmpv
            character(*) :: ncmpva
            character(*) :: ncmpvm
            integer :: iinst
            integer :: numpt
            integer :: numord
            real(kind=8) :: inst
            character(len=8) :: crit
            real(kind=8) :: prec
            character(len=8) :: nomgd
            integer :: ncmprf
            integer :: jnocmp
            character(len=19) :: chames
            integer :: codret
          end subroutine lrcame
        end interface
