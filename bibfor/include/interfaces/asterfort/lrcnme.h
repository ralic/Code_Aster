        interface
          subroutine lrcnme(chanom,nochmd,nomamd,nomaas,nomgd,typent,&
     &nbcmpv,ncmpva,ncmpvm,iinst,numpt,numord,inst,crit,prec,nrofic,&
     &codret)
            character(*) :: chanom
            character(*) :: nochmd
            character(*) :: nomamd
            character(len=8) :: nomaas
            character(len=8) :: nomgd
            integer :: typent
            integer :: nbcmpv
            character(*) :: ncmpva
            character(*) :: ncmpvm
            integer :: iinst
            integer :: numpt
            integer :: numord
            real(kind=8) :: inst
            character(len=8) :: crit
            real(kind=8) :: prec
            integer :: nrofic
            integer :: codret
          end subroutine lrcnme
        end interface
