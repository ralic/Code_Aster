        interface
          subroutine assma3(lmasym,lmesym,tt,igr,iel,c1,rang,ifel2,&
     &ifel3,ifel4,ifel5,ifm,jfnusd,jnueq,jnumsd,jresl,jrsvi,nbvel,nnoe,&
     &lfeti,llich,llichd,llichp,llimo,ldist,ldgrel,ilima,jadli,jadne,&
     &jprn1,jprn2,jnulo1,jnulo2,jposd1,jposd2,admodl,lcmodl,mode,nec,&
     &nmxcmp,ncmp,jsmhc,jsmdi,iconx1,iconx2,ligre1,ligre2,infofe,jtmp2,&
     &lgtmp2,jvalm,ilinu,idd,ellagr,exivf,jdesc,jrepe,jptvoi,jelvoi,&
     &codvoi)
            logical :: lmasym
            logical :: lmesym
            character(len=2) :: tt
            integer :: igr
            integer :: iel
            real(kind=8) :: c1
            integer :: rang
            integer :: ifel2
            integer :: ifel3
            integer :: ifel4
            integer :: ifel5
            integer :: ifm
            integer :: jfnusd
            integer :: jnueq
            integer :: jnumsd
            integer :: jresl
            integer :: jrsvi
            integer :: nbvel
            integer :: nnoe
            logical :: lfeti
            logical :: llich
            logical :: llichd
            logical :: llichp
            logical :: llimo
            logical :: ldist
            logical :: ldgrel
            integer :: ilima
            integer :: jadli
            integer :: jadne
            integer :: jprn1
            integer :: jprn2
            integer :: jnulo1
            integer :: jnulo2
            integer :: jposd1
            integer :: jposd2
            integer :: admodl
            integer :: lcmodl
            integer :: mode
            integer :: nec
            integer :: nmxcmp
            integer :: ncmp
            integer :: jsmhc
            integer :: jsmdi
            integer :: iconx1
            integer :: iconx2
            character(len=19) :: ligre1
            character(len=19) :: ligre2
            character(len=24) :: infofe
            integer :: jtmp2
            integer :: lgtmp2
            integer :: jvalm(2)
            integer :: ilinu
            integer :: idd
            integer :: ellagr
            character(*) :: exivf
            integer :: jdesc
            integer :: jrepe
            integer :: jptvoi
            integer :: jelvoi
            character(len=16) :: codvoi
          end subroutine assma3
        end interface
