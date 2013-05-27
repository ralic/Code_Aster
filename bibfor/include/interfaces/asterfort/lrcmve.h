        interface
          subroutine lrcmve(ntvale,nmatyp,nbnoma,ntproa,lgproa,ncmprf,&
     &nomcmr,ntypel,npgmax,indpg,nbcmfi,nmcmfi,nbcmpv,ncmpvm,numcmp,&
     &jnumma,nochmd,nbma,npgma,npgmm,typech,nutyma,adsl,adsv,adsd,codret&
     &)
            integer :: nbma
            integer :: npgmax
            integer :: ntypel
            character(*) :: ntvale
            integer :: nmatyp
            integer :: nbnoma
            character(*) :: ntproa
            integer :: lgproa
            integer :: ncmprf
            character(*) :: nomcmr(*)
            integer :: indpg(ntypel,npgmax)
            integer :: nbcmfi
            character(*) :: nmcmfi
            integer :: nbcmpv
            character(*) :: ncmpvm
            character(*) :: numcmp
            integer :: jnumma
            character(*) :: nochmd
            integer :: npgma(nbma)
            integer :: npgmm(nbma)
            character(*) :: typech
            integer :: nutyma
            integer :: adsl
            integer :: adsv
            integer :: adsd
            integer :: codret
          end subroutine lrcmve
        end interface
