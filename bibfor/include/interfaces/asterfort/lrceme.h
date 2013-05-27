        interface
          subroutine lrceme(chanom,nochmd,typech,nomamd,nomaas,nommod,&
     &nomgd,typent,nbcmpv,ncmpva,ncmpvm,prolz,iinst,numpt,numord,inst,&
     &crit,prec,nrofic,option,param,nbpgma,nbpgmm,codret)
            character(len=19) :: chanom
            character(*) :: nochmd
            character(len=4) :: typech
            character(*) :: nomamd
            character(len=8) :: nomaas
            character(len=8) :: nommod
            character(len=8) :: nomgd
            integer :: typent
            integer :: nbcmpv
            character(*) :: ncmpva
            character(*) :: ncmpvm
            character(len=3) :: prolz
            integer :: iinst
            integer :: numpt
            integer :: numord
            real(kind=8) :: inst
            character(len=8) :: crit
            real(kind=8) :: prec
            integer :: nrofic
            character(len=24) :: option
            character(len=8) :: param
            integer :: nbpgma(*)
            integer :: nbpgmm(*)
            integer :: codret
          end subroutine lrceme
        end interface
